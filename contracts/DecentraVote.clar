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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

    (award-reputation-points tx-sender u10 "proposal-creation")


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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

    ;; Check if voting is still open
    (asserts! (<= current-block (get end-block-height proposal)) ERR-VOTING-CLOSED)
    
    ;; Check if user has staked tokens
    (asserts! (> (get amount user-stake) u0) ERR-INSUFFICIENT-TOKENS)
    
    ;; Check if user has already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    (award-reputation-points tx-sender u5 "voting")

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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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
    (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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
  (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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
        (asserts! (not (var-get contract-paused)) ERR-PAUSED)

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


(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-OWNER (err u109))
(define-constant ERR-PAUSED (err u110))

(define-data-var contract-paused bool false)

(define-public (toggle-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-OWNER)
        (ok (var-set contract-paused (not (var-get contract-paused))))
    )
)

(define-read-only (is-paused)
    (var-get contract-paused)
)


(define-constant ERR-ENDORSEMENT-EXISTS (err u111))
(define-constant ERR-INVALID-PROPOSAL-STATE (err u112))

(define-map proposal-endorsements
    { proposal-id: uint, endorser: principal }
    { endorsed: bool }
)

(define-map endorsement-counts
    { proposal-id: uint }
    { count: uint }
)

(define-public (endorse-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
            (current-block stacks-block-height)
        )
        (asserts! (< current-block (get start-block-height proposal)) ERR-INVALID-PROPOSAL-STATE)
        (asserts! (is-none (map-get? proposal-endorsements { proposal-id: proposal-id, endorser: tx-sender })) ERR-ENDORSEMENT-EXISTS)
        
        (map-set proposal-endorsements
            { proposal-id: proposal-id, endorser: tx-sender }
            { endorsed: true }
        )
        
        (map-set endorsement-counts
            { proposal-id: proposal-id }
            { count: (+ (get count (default-to { count: u0 } (map-get? endorsement-counts { proposal-id: proposal-id }))) u1) }
        )
        (ok true)
    )
)

(define-read-only (get-endorsement-count (proposal-id uint))
    (default-to { count: u0 } (map-get? endorsement-counts { proposal-id: proposal-id }))
)


(define-constant ERR-MILESTONE-EXISTS (err u113))
(define-constant ERR-MILESTONE-NOT-FOUND (err u114))
(define-constant ERR-INVALID-MILESTONE-STATE (err u115))

(define-map proposal-milestones
    { proposal-id: uint, milestone-id: uint }
    {
        title: (string-utf8 100),
        description: (string-utf8 200),
        deadline: uint,
        completed: bool
    }
)

(define-map milestone-counts
    { proposal-id: uint }
    { count: uint }
)

(define-public (add-milestone 
    (proposal-id uint) 
    (title (string-utf8 100)) 
    (description (string-utf8 200)) 
    (deadline uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
            (milestone-count (default-to { count: u0 } (map-get? milestone-counts { proposal-id: proposal-id })))
        )
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
        
        (map-set proposal-milestones
            { proposal-id: proposal-id, milestone-id: (get count milestone-count) }
            {
                title: title,
                description: description,
                deadline: deadline,
                completed: false
            }
        )
        
        (map-set milestone-counts
            { proposal-id: proposal-id }
            { count: (+ (get count milestone-count) u1) }
        )
        (ok (get count milestone-count))
    )
)

(define-public (complete-milestone (proposal-id uint) (milestone-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
            (milestone (unwrap! (map-get? proposal-milestones { proposal-id: proposal-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed milestone)) ERR-INVALID-MILESTONE-STATE)
        
        (map-set proposal-milestones
            { proposal-id: proposal-id, milestone-id: milestone-id }
            (merge milestone { completed: true })
        )
        (ok true)
    )
)

(define-read-only (get-milestone (proposal-id uint) (milestone-id uint))
    (map-get? proposal-milestones { proposal-id: proposal-id, milestone-id: milestone-id })
)

(define-read-only (get-milestone-count (proposal-id uint))
    (default-to { count: u0 } (map-get? milestone-counts { proposal-id: proposal-id }))
)

(define-constant ERR-REPUTATION-OVERFLOW (err u116))

(define-map user-reputation
  { user: principal }
  {
    total-points: uint,
    proposals-created: uint,
    successful-proposals: uint,
    votes-cast: uint,
    majority-votes: uint,
    participation-streak: uint,
    last-activity-block: uint
  }
)

(define-map reputation-levels
  { level: uint }
  {
    name: (string-utf8 20),
    min-points: uint,
    voting-bonus: uint
  }
)

(define-data-var reputation-initialized bool false)

(define-private (initialize-reputation-levels)
  (begin
    (map-set reputation-levels { level: u1 } { name: u"Newcomer", min-points: u0, voting-bonus: u0 })
    (map-set reputation-levels { level: u2 } { name: u"Contributor", min-points: u100, voting-bonus: u5 })
    (map-set reputation-levels { level: u3 } { name: u"Veteran", min-points: u500, voting-bonus: u10 })
    (map-set reputation-levels { level: u4 } { name: u"Expert", min-points: u1000, voting-bonus: u20 })
    (map-set reputation-levels { level: u5 } { name: u"Guardian", min-points: u2500, voting-bonus: u50 })
    (var-set reputation-initialized true)
  )
)

(define-private (ensure-reputation-initialized)
  (if (not (var-get reputation-initialized))
    (initialize-reputation-levels)
    true
  )
)

(define-private (get-user-reputation-data (user principal))
  (default-to 
    {
      total-points: u0,
      proposals-created: u0,
      successful-proposals: u0,
      votes-cast: u0,
      majority-votes: u0,
      participation-streak: u0,
      last-activity-block: u0
    }
    (map-get? user-reputation { user: user })
  )
)

(define-private (update-participation-streak (user principal) (current-block uint))
  (let
    (
      (user-rep (get-user-reputation-data user))
      (last-block (get last-activity-block user-rep))
      (block-diff (- current-block last-block))
    )
    (if (and (> last-block u0) (<= block-diff u2016))
      (+ (get participation-streak user-rep) u1)
      u1
    )
  )
)

(define-private (award-reputation-points (user principal) (points uint) (activity-type (string-ascii 20)))
  (let
    (
      (current-rep (get-user-reputation-data user))
      (current-block stacks-block-height)
      (new-streak (update-participation-streak user current-block))
      (streak-bonus (if (> new-streak u10) u10 u0))
      (total-new-points (+ points streak-bonus))
    )
    (ensure-reputation-initialized)
    (map-set user-reputation
      { user: user }
      (merge current-rep 
        {
          total-points: (+ (get total-points current-rep) total-new-points),
          participation-streak: new-streak,
          last-activity-block: current-block
        }
      )
    )
  )
)

(define-private (update-proposal-reputation (user principal) (successful bool))
  (let
    (
      (current-rep (get-user-reputation-data user))
      (points (if successful u50 u10))
    )
    (map-set user-reputation
      { user: user }
      (merge current-rep
        {
          proposals-created: (+ (get proposals-created current-rep) u1),
          successful-proposals: (if successful 
            (+ (get successful-proposals current-rep) u1)
            (get successful-proposals current-rep)
          )
        }
      )
    )
    (award-reputation-points user points "proposal")
  )
)

(define-private (update-voting-reputation (user principal) (voted-with-majority bool))
  (let
    (
      (current-rep (get-user-reputation-data user))
      (points (if voted-with-majority u20 u5))
    )
    (map-set user-reputation
      { user: user }
      (merge current-rep
        {
          votes-cast: (+ (get votes-cast current-rep) u1),
          majority-votes: (if voted-with-majority
            (+ (get majority-votes current-rep) u1)
            (get majority-votes current-rep)
          )
        }
      )
    )
    (award-reputation-points user points "voting")
  )
)

(define-public (finalize-proposal-reputation (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (proposal-passed (> (get yes-votes proposal) (get no-votes proposal)))
    )
    (asserts! (get executed proposal) ERR-INVALID-PROPOSAL-STATE)
    (update-proposal-reputation (get creator proposal) proposal-passed)
    (ok proposal-passed)
  )
)

(define-public (update-voter-reputation (proposal-id uint) (voter principal))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (vote-data (unwrap! (map-get? votes { proposal-id: proposal-id, voter: voter }) ERR-PROPOSAL-NOT-FOUND))
      (proposal-passed (> (get yes-votes proposal) (get no-votes proposal)))
      (voted-with-majority (is-eq (get vote vote-data) proposal-passed))
    )
    (asserts! (get executed proposal) ERR-INVALID-PROPOSAL-STATE)
    (update-voting-reputation voter voted-with-majority)
    (ok voted-with-majority)
  )
)

(define-read-only (get-user-reputation (user principal))
  (let
    (
      (user-rep (get-user-reputation-data user))
    )
    (ok user-rep)
  )
)

(define-read-only (get-user-level (user principal))
  (let
    (
      (user-rep (get-user-reputation-data user))
      (points (get total-points user-rep))
    )
    (if (>= points u2500) u5
      (if (>= points u1000) u4
        (if (>= points u500) u3
          (if (>= points u100) u2 u1)
        )
      )
    )
  )
)

(define-read-only (get-level-info (level uint))
  (begin
    (map-get? reputation-levels { level: level })
  )
)

(define-read-only (get-user-voting-accuracy (user principal))
  (let
    (
      (user-rep (get-user-reputation-data user))
      (total-votes (get votes-cast user-rep))
      (majority-votes (get majority-votes user-rep))
    )
    (if (> total-votes u0)
      (some (/ (* majority-votes u100) total-votes))
      none
    )
  )
)

(define-read-only (get-user-proposal-success-rate (user principal))
  (let
    (
      (user-rep (get-user-reputation-data user))
      (total-proposals (get proposals-created user-rep))
      (successful-proposals (get successful-proposals user-rep))
    )
    (if (> total-proposals u0)
      (some (/ (* successful-proposals u100) total-proposals))
      none
    )
  )
)

(define-read-only (calculate-reputation-bonus (user principal))
  (let
    (
      (user-level (get-user-level user))
      (level-info (unwrap! (get-level-info user-level) u0))
    )
    (get voting-bonus level-info)
  )
)