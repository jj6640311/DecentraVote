;; title: DecentraVote
;; version: 1.0
;; summary: A secure voting platform for DAOs and organizations
;; description: Allows token-weighted voting, proposal tracking, and result calculation

;; constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-TOKENS (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-PROPOSAL-ACTIVE (err u106))

;; data vars
(define-data-var proposal-count uint u0)

;; data maps
;; Map to store proposal details
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    start-block-height: uint,
    end-block-height: uint,
    executed: bool,
    yes-votes: uint,
    no-votes: uint,
    min-tokens-to-create: uint
  }
)

;; Map to track tokens staked by users for voting
(define-map user-stakes
  { user: principal }
  { amount: uint }
)

;; Map to track votes cast by users
(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

;; public functions

;; Create a new proposal
(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 500)) (duration uint) (min-tokens uint))
  (let
    (
      (proposal-id (var-get proposal-count))
      (user-stake (default-to { amount: u0 } (map-get? user-stakes { user: tx-sender })))
      (start-block stacks-block-height)
      (end-block (+ start-block duration))
    )
    
    ;; Check if user has staked enough tokens to create a proposal
    (asserts! (>= (get amount user-stake) min-tokens) ERR-INSUFFICIENT-TOKENS)
    
    ;; Store the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        start-block-height: start-block,
        end-block-height: end-block,
        executed: false,
        yes-votes: u0,
        no-votes: u0,
        min-tokens-to-create: min-tokens
      }
    )
    
    ;; Increment proposal count
    (var-set proposal-count (+ proposal-id u1))
    
    (ok proposal-id)
  )
)

;; Stake tokens to participate in voting
(define-public (stake-tokens (amount uint))
  (let
    (
      (current-stake (default-to { amount: u0 } (map-get? user-stakes { user: tx-sender })))
      (new-amount (+ (get amount current-stake) amount))
    )
    
    ;; Update user stake
    (map-set user-stakes
      { user: tx-sender }
      { amount: new-amount }
    )
    
    (ok new-amount)
  )
)

;; Unstake tokens (only if not actively voting)
(define-public (unstake-tokens (amount uint))
  (let
    (
      (current-stake (default-to { amount: u0 } (map-get? user-stakes { user: tx-sender })))
    )
    
    ;; Check if user has enough staked tokens
    (asserts! (>= (get amount current-stake) amount) ERR-INSUFFICIENT-TOKENS)
    
    ;; Update user stake
    (map-set user-stakes
      { user: tx-sender }
      { amount: (- (get amount current-stake) amount) }
    )
    
    (ok true)
  )
)

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-value bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (user-stake (default-to { amount: u0 } (map-get? user-stakes { user: tx-sender })))
      (current-block stacks-block-height)
    )
    
    ;; Check if voting is still open
    (asserts! (<= current-block (get end-block-height proposal)) ERR-VOTING-CLOSED)
    
    ;; Check if user has staked tokens
    (asserts! (> (get amount user-stake) u0) ERR-INSUFFICIENT-TOKENS)
    
    ;; Check if user has already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-value, weight: (get amount user-stake) }
    )
    
    ;; Update vote tallies
    (if vote-value
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { yes-votes: (+ (get yes-votes proposal) (get amount user-stake)) })
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { no-votes: (+ (get no-votes proposal) (get amount user-stake)) })
      )
    )
    
    (ok true)
  )
)

;; Execute a proposal after voting ends
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (current-block stacks-block-height)
    )
    
    ;; Check if voting period has ended
    (asserts! (> current-block (get end-block-height proposal)) ERR-PROPOSAL-ACTIVE)
    
    ;; Check if proposal has not been executed yet
    (asserts! (not (get executed proposal)) ERR-NOT-AUTHORIZED)
    
    ;; Mark proposal as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    ;; Return the result of the vote
    (ok (> (get yes-votes proposal) (get no-votes proposal)))
  )
)

;; read only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get user stake
(define-read-only (get-user-stake (user principal))
  (default-to { amount: u0 } (map-get? user-stakes { user: user }))
)

;; Get user vote on a specific proposal
(define-read-only (get-user-vote (proposal-id uint) (user principal))
  (map-get? votes { proposal-id: proposal-id, voter: user })
)

;; Get total number of proposals
(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

;; Check if a proposal passed
(define-read-only (proposal-passed (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    (ok (> (get yes-votes proposal) (get no-votes proposal)))
  )
)
