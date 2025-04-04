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
(define-constant ERR-COMMENT-TOO-LONG (err u107))

;; data vars
(define-data-var proposal-count uint u0)
(define-data-var comment-count uint u0)

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
    min-tokens-to-create: uint,
    quorum: uint  ;; Add this field
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

;; Map to store proposal categories
(define-map proposal-categories
  { proposal-id: uint }
  { category: (string-utf8 20) }
)

;; Map to store proposal comments
(define-map proposal-comments
  { proposal-id: uint, comment-id: uint }
  {
    author: principal,
    content: (string-utf8 200),
    block-height: uint
  }
)

;; Map to store delegations
(define-map delegations
  { delegator: principal }
  { delegate: principal }
)

;; public functions

;; Create a new proposal
(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 500)) (duration uint) (min-tokens uint) (quorum uint))
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
        min-tokens-to-create: min-tokens,
        quorum: quorum
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

;; Set proposal category
(define-public (set-proposal-category (proposal-id uint) (category (string-utf8 20)))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    ;; Only creator can set category
    (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
    
    (map-set proposal-categories
      { proposal-id: proposal-id }
      { category: category }
    )
    (ok true)
  )
)

;; Add comment to proposal
(define-public (add-comment (proposal-id uint) (content (string-utf8 200)))
  (let
    (
      (comment-id (var-get comment-count))
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    )
    
    (map-set proposal-comments
      { proposal-id: proposal-id, comment-id: comment-id }
      {
        author: tx-sender,
        content: content,
        block-height: stacks-block-height
      }
    )
    
    (var-set comment-count (+ comment-id u1))
    (ok comment-id)
  )
)

;; Delegate vote
(define-public (delegate-vote (delegate-to principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate-to)) ERR-NOT-AUTHORIZED)
    (map-set delegations
      { delegator: tx-sender }
      { delegate: delegate-to }
    )
    (ok true)
  )
)

;; Remove delegation
(define-public (remove-delegation)
  (begin
    (map-delete delegations { delegator: tx-sender })
    (ok true)
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

;; Get proposal category
(define-read-only (get-proposal-category (proposal-id uint))
  (map-get? proposal-categories { proposal-id: proposal-id })
)

;; Get comment details
(define-read-only (get-comment (proposal-id uint) (comment-id uint))
  (map-get? proposal-comments { proposal-id: proposal-id, comment-id: comment-id })
)

;; Get delegate
(define-read-only (get-delegate (user principal))
  (map-get? delegations { delegator: user })
)

;; Check if quorum is reached
(define-read-only (check-quorum-reached (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
    )
    (ok (>= total-votes (get quorum proposal)))
  )
)




;; Add to constants
(define-constant ERR-CANNOT-CANCEL (err u108))

;; Add new public function
(define-public (cancel-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
            (current-block stacks-block-height)
        )
        ;; Only creator can cancel and only before voting ends
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (<= current-block (get end-block-height proposal)) ERR-CANNOT-CANCEL)
        (asserts! (not (get executed proposal)) ERR-CANNOT-CANCEL)
        
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { executed: true })
        )
        (ok true)
    )
)

;; Add to data maps
(define-map token-locks
    { user: principal }
    {
        amount: uint,
        lock-until: uint,
        multiplier: uint
    }
)

;; Add new public function
(define-public (lock-tokens (amount uint) (duration uint))
    (let
        (
            (current-block stacks-block-height)
            (multiplier (if (>= duration u50000) u2 u1))
            (lock-end (+ current-block duration))
        )
        
        (map-set token-locks
            { user: tx-sender }
            {
                amount: amount,
                lock-until: lock-end,
                multiplier: multiplier
            }
        )
        (ok true)
    )
)

;; Add new read-only function
(define-read-only (get-voting-power (user principal))
    (let
        (
            (lock-info (default-to { amount: u0, lock-until: u0, multiplier: u1 } 
                (map-get? token-locks { user: user })))
        )
        (ok (* (get amount lock-info) (get multiplier lock-info)))
    )
)

;; Add to data maps
(define-map proposal-templates
    { template-id: uint }
    {
        name: (string-utf8 50),
        description: (string-utf8 500),
        duration: uint,
        min-tokens: uint,
        quorum: uint
    }
)

(define-data-var template-count uint u0)

;; Add new public functions
(define-public (create-template 
    (name (string-utf8 50))
    (description (string-utf8 500))
    (duration uint)
    (min-tokens uint)
    (quorum uint))
    (let
        (
            (template-id (var-get template-count))
        )
        (map-set proposal-templates
            { template-id: template-id }
            {
                name: name,
                description: description,
                duration: duration,
                min-tokens: min-tokens,
                quorum: quorum
            }
        )
        (var-set template-count (+ template-id u1))
        (ok template-id)
    )
)

;; Add new read-only function
(define-read-only (get-template (template-id uint))
    (map-get? proposal-templates { template-id: template-id })
)