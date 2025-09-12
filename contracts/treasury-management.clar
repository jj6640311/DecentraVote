;; Treasury Management & Automated Distribution System
;; This contract manages organizational treasuries and automates fund distribution based on proposal outcomes

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-TREASURY-NOT-FOUND (err u301))
(define-constant ERR-TREASURY-EXISTS (err u302))
(define-constant ERR-INSUFFICIENT-FUNDS (err u303))
(define-constant ERR-INVALID-AMOUNT (err u304))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u305))
(define-constant ERR-DISTRIBUTION-NOT-FOUND (err u306))
(define-constant ERR-ALREADY-DISTRIBUTED (err u307))
(define-constant ERR-PROPOSAL-NOT-EXECUTED (err u308))
(define-constant ERR-PROPOSAL-NOT_PASSED (err u309))
(define-constant ERR-INVALID-DISTRIBUTION-TYPE (err u310))
(define-constant ERR-TREASURY-LOCKED (err u311))
(define-constant ERR-INVALID-THRESHOLD (err u312))

;; Data Variables
(define-data-var next-treasury-id uint u1)
(define-data-var next-distribution-id uint u1)
(define-data-var total-treasuries uint u0)
(define-data-var contract-paused bool false)

;; Data Maps

;; Treasury storage
(define-map treasuries
    uint
    {
        owner: principal,
        name: (string-utf8 100),
        description: (string-utf8 300),
        total-balance: uint,
        available-balance: uint,
        locked-balance: uint,
        created-at: uint,
        active: bool,
        auto-distribute: bool,
        min-proposal-threshold: uint
    }
)

;; Treasury access control
(define-map treasury-managers
    {treasury-id: uint, manager: principal}
    {
        permissions: (string-ascii 20), ;; "admin", "withdraw", "view"
        granted-by: principal,
        granted-at: uint
    }
)

;; Fund distribution rules per treasury
(define-map distribution-rules
    uint ;; treasury-id
    {
        voter-reward-percentage: uint, ;; 0-100
        proposal-creator-percentage: uint, ;; 0-100
        treasury-reserve-percentage: uint, ;; 0-100
        min-participation-threshold: uint,
        reward-decay-factor: uint ;; For time-based reward reduction
    }
)

;; Proposal-specific distribution configurations
(define-map proposal-distributions
    uint ;; distribution-id
    {
        treasury-id: uint,
        proposal-id: uint,
        total-amount: uint,
        distribution-type: (string-ascii 20), ;; "success-reward", "participation", "milestone"
        recipient-count: uint,
        distributed-amount: uint,
        created-at: uint,
        executed: bool,
        conditions-met: bool
    }
)

;; Individual reward claims
(define-map reward-claims
    {distribution-id: uint, recipient: principal}
    {
        amount: uint,
        claimed: bool,
        claim-reason: (string-ascii 30), ;; "voter-reward", "creator-bonus", "participation"
        calculated-at: uint
    }
)

;; Treasury transaction log
(define-map treasury-transactions
    {treasury-id: uint, tx-id: uint}
    {
        transaction-type: (string-ascii 20), ;; "deposit", "withdraw", "distribution", "lock"
        amount: uint,
        from-address: (optional principal),
        to-address: (optional principal),
        description: (string-utf8 200),
        block-height: uint,
        related-proposal: (optional uint)
    }
)

(define-map treasury-tx-counters
    uint ;; treasury-id
    uint ;; counter
)

;; Treasury Fund Management Functions

;; Create a new treasury
(define-public (create-treasury 
    (name (string-utf8 100)) 
    (description (string-utf8 300))
    (min-proposal-threshold uint)
    (auto-distribute bool))
    (let 
        (
            (treasury-id (var-get next-treasury-id))
        )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (> min-proposal-threshold u0) ERR-INVALID-THRESHOLD)
        
        ;; Create treasury
        (map-set treasuries treasury-id
            {
                owner: tx-sender,
                name: name,
                description: description,
                total-balance: u0,
                available-balance: u0,
                locked-balance: u0,
                created-at: stacks-block-height,
                active: true,
                auto-distribute: auto-distribute,
                min-proposal-threshold: min-proposal-threshold
            }
        )
        
        ;; Grant admin permissions to creator
        (map-set treasury-managers {treasury-id: treasury-id, manager: tx-sender}
            {
                permissions: "admin",
                granted-by: tx-sender,
                granted-at: stacks-block-height
            }
        )
        
        ;; Initialize transaction counter
        (map-set treasury-tx-counters treasury-id u0)
        
        ;; Set default distribution rules
        (map-set distribution-rules treasury-id
            {
                voter-reward-percentage: u70,
                proposal-creator-percentage: u20,
                treasury-reserve-percentage: u10,
                min-participation-threshold: u5,
                reward-decay-factor: u1
            }
        )
        
        (var-set next-treasury-id (+ treasury-id u1))
        (var-set total-treasuries (+ (var-get total-treasuries) u1))
        
        (ok treasury-id)
    )
)

;; Deposit funds into treasury
(define-public (deposit-to-treasury (treasury-id uint) (amount uint))
    (let 
        (
            (treasury-data (unwrap! (map-get? treasuries treasury-id) ERR-TREASURY-NOT-FOUND))
            (tx-count (default-to u0 (map-get? treasury-tx-counters treasury-id)))
        )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (get active treasury-data) ERR-TREASURY-LOCKED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update treasury balances
        (map-set treasuries treasury-id
            (merge treasury-data {
                total-balance: (+ (get total-balance treasury-data) amount),
                available-balance: (+ (get available-balance treasury-data) amount)
            })
        )
        
        ;; Log transaction
        (map-set treasury-transactions {treasury-id: treasury-id, tx-id: tx-count}
            {
                transaction-type: "deposit",
                amount: amount,
                from-address: (some tx-sender),
                to-address: none,
                description: u"Treasury deposit",
                block-height: stacks-block-height,
                related-proposal: none
            }
        )
        
        (map-set treasury-tx-counters treasury-id (+ tx-count u1))
        
        (ok true)
    )
)

;; Configure distribution rules for treasury
(define-public (set-distribution-rules
    (treasury-id uint)
    (voter-reward-pct uint)
    (creator-pct uint)
    (reserve-pct uint)
    (min-participation uint)
    (decay-factor uint))
    (let 
        (
            (treasury-data (unwrap! (map-get? treasuries treasury-id) ERR-TREASURY-NOT-FOUND))
            (manager-perms (map-get? treasury-managers {treasury-id: treasury-id, manager: tx-sender}))
        )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (or (is-eq tx-sender (get owner treasury-data))
                     (and (is-some manager-perms)
                          (is-eq (get permissions (unwrap-panic manager-perms)) "admin")))
                 ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (+ voter-reward-pct creator-pct reserve-pct) u100) ERR-INVALID-AMOUNT)
        
        (map-set distribution-rules treasury-id
            {
                voter-reward-percentage: voter-reward-pct,
                proposal-creator-percentage: creator-pct,
                treasury-reserve-percentage: reserve-pct,
                min-participation-threshold: min-participation,
                reward-decay-factor: decay-factor
            }
        )
        
        (ok true)
    )
)

;; Create distribution for successful proposal
(define-public (create-proposal-distribution 
    (treasury-id uint)
    (proposal-id uint)
    (distribution-amount uint)
    (distribution-type (string-ascii 20)))
    (let 
        (
            (treasury-data (unwrap! (map-get? treasuries treasury-id) ERR-TREASURY-NOT-FOUND))
            (distribution-id (var-get next-distribution-id))
            (manager-perms (map-get? treasury-managers {treasury-id: treasury-id, manager: tx-sender}))
        )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (or (is-eq tx-sender (get owner treasury-data))
                     (and (is-some manager-perms)
                          (or (is-eq (get permissions (unwrap-panic manager-perms)) "admin")
                              (is-eq (get permissions (unwrap-panic manager-perms)) "withdraw"))))
                 ERR-NOT-AUTHORIZED)
        (asserts! (>= (get available-balance treasury-data) distribution-amount) ERR-INSUFFICIENT-FUNDS)
        (asserts! (or (is-eq distribution-type "success-reward")
                     (is-eq distribution-type "participation")
                     (is-eq distribution-type "milestone")) ERR-INVALID-DISTRIBUTION-TYPE)
        
        ;; Create distribution record
        (map-set proposal-distributions distribution-id
            {
                treasury-id: treasury-id,
                proposal-id: proposal-id,
                total-amount: distribution-amount,
                distribution-type: distribution-type,
                recipient-count: u0,
                distributed-amount: u0,
                created-at: stacks-block-height,
                executed: false,
                conditions-met: false
            }
        )
        
        ;; Lock funds in treasury
        (map-set treasuries treasury-id
            (merge treasury-data {
                available-balance: (- (get available-balance treasury-data) distribution-amount),
                locked-balance: (+ (get locked-balance treasury-data) distribution-amount)
            })
        )
        
        (var-set next-distribution-id (+ distribution-id u1))
        
        (ok distribution-id)
    )
)

;; Process automatic distribution after proposal execution
(define-public (execute-distribution (distribution-id uint) (voters (list 50 principal)))
    (let 
        (
            (distribution-data (unwrap! (map-get? proposal-distributions distribution-id) ERR-DISTRIBUTION-NOT-FOUND))
            (treasury-id (get treasury-id distribution-data))
            (treasury-data (unwrap! (map-get? treasuries treasury-id) ERR-TREASURY-NOT-FOUND))
            (rules (unwrap! (map-get? distribution-rules treasury-id) ERR-TREASURY-NOT-FOUND))
            (total-amount (get total-amount distribution-data))
        )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get executed distribution-data)) ERR-ALREADY-DISTRIBUTED)
        
        ;; Check if conditions are met (simplified - would normally verify proposal passed)
        (asserts! (>= (len voters) (get min-participation-threshold rules)) ERR-INVALID-AMOUNT)
        
        ;; Calculate reward amounts
        (let 
            (
                (voter-pool (* total-amount (get voter-reward-percentage rules)))
                (individual-reward (/ voter-pool (len voters)))
            )
            ;; Process voter rewards
            (process-voter-rewards distribution-id voters individual-reward)
            
            ;; Mark distribution as executed
            (map-set proposal-distributions distribution-id
                (merge distribution-data {
                    executed: true,
                    recipient-count: (len voters),
                    distributed-amount: voter-pool,
                    conditions-met: true
                })
            )
            
            ;; Update treasury balances
            (map-set treasuries treasury-id
                (merge treasury-data {
                    locked-balance: (- (get locked-balance treasury-data) total-amount)
                })
            )
            
            (ok true)
        )
    )
)

;; Helper function to process voter rewards
(define-private (process-voter-rewards (distribution-id uint) (voters (list 50 principal)) (individual-amount uint))
    (map process-single-voter-reward 
         (map create-voter-reward-data voters)
    )
)

(define-private (create-voter-reward-data (voter principal))
    { voter: voter, amount: u100 } ;; Simplified amount
)

(define-private (process-single-voter-reward (reward-data { voter: principal, amount: uint }))
    ;; Simplified reward processing
    true
)

;; Claim individual rewards
(define-public (claim-reward (distribution-id uint))
    (let 
        (
            (reward-data (unwrap! (map-get? reward-claims {distribution-id: distribution-id, recipient: tx-sender}) ERR-DISTRIBUTION-NOT-FOUND))
        )
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get claimed reward-data)) ERR-ALREADY-DISTRIBUTED)
        (asserts! (> (get amount reward-data) u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer reward
        (try! (as-contract (stx-transfer? (get amount reward-data) tx-sender tx-sender)))
        
        ;; Mark as claimed
        (map-set reward-claims {distribution-id: distribution-id, recipient: tx-sender}
            (merge reward-data { claimed: true })
        )
        
        (ok (get amount reward-data))
    )
)

;; Treasury management functions

;; Grant manager permissions
(define-public (grant-treasury-access 
    (treasury-id uint)
    (manager principal)
    (permissions (string-ascii 20)))
    (let 
        (
            (treasury-data (unwrap! (map-get? treasuries treasury-id) ERR-TREASURY-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get owner treasury-data)) ERR-NOT-AUTHORIZED)
        (asserts! (or (is-eq permissions "admin")
                     (is-eq permissions "withdraw")
                     (is-eq permissions "view")) ERR-INVALID-DISTRIBUTION-TYPE)
        
        (map-set treasury-managers {treasury-id: treasury-id, manager: manager}
            {
                permissions: permissions,
                granted-by: tx-sender,
                granted-at: stacks-block-height
            }
        )
        
        (ok true)
    )
)

;; Emergency withdraw (owner only)
(define-public (emergency-withdraw (treasury-id uint) (amount uint) (recipient principal))
    (let 
        (
            (treasury-data (unwrap! (map-get? treasuries treasury-id) ERR-TREASURY-NOT-FOUND))
            (tx-count (default-to u0 (map-get? treasury-tx-counters treasury-id)))
        )
        (asserts! (is-eq tx-sender (get owner treasury-data)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get available-balance treasury-data) amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer funds
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        
        ;; Update treasury balance
        (map-set treasuries treasury-id
            (merge treasury-data {
                available-balance: (- (get available-balance treasury-data) amount),
                total-balance: (- (get total-balance treasury-data) amount)
            })
        )
        
        ;; Log transaction
        (map-set treasury-transactions {treasury-id: treasury-id, tx-id: tx-count}
            {
                transaction-type: "withdraw",
                amount: amount,
                from-address: none,
                to-address: (some recipient),
                description: u"Emergency withdrawal",
                block-height: stacks-block-height,
                related-proposal: none
            }
        )
        
        (map-set treasury-tx-counters treasury-id (+ tx-count u1))
        
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-treasury (treasury-id uint))
    (map-get? treasuries treasury-id)
)

(define-read-only (get-distribution-rules (treasury-id uint))
    (map-get? distribution-rules treasury-id)
)

(define-read-only (get-proposal-distribution (distribution-id uint))
    (map-get? proposal-distributions distribution-id)
)

(define-read-only (get-reward-claim (distribution-id uint) (recipient principal))
    (map-get? reward-claims {distribution-id: distribution-id, recipient: recipient})
)

(define-read-only (get-treasury-manager-permissions (treasury-id uint) (manager principal))
    (map-get? treasury-managers {treasury-id: treasury-id, manager: manager})
)

(define-read-only (get-treasury-transaction (treasury-id uint) (tx-id uint))
    (map-get? treasury-transactions {treasury-id: treasury-id, tx-id: tx-id})
)

(define-read-only (get-treasury-stats)
    (ok {
        total-treasuries: (var-get total-treasuries),
        next-treasury-id: (var-get next-treasury-id),
        next-distribution-id: (var-get next-distribution-id),
        contract-paused: (var-get contract-paused)
    })
)

(define-read-only (calculate-distribution-amounts (treasury-id uint) (total-amount uint))
    (let 
        (
            (rules (unwrap! (map-get? distribution-rules treasury-id) ERR-TREASURY-NOT-FOUND))
        )
        (ok {
            voter-reward-pool: (/ (* total-amount (get voter-reward-percentage rules)) u100),
            creator-bonus: (/ (* total-amount (get proposal-creator-percentage rules)) u100),
            treasury-reserve: (/ (* total-amount (get treasury-reserve-percentage rules)) u100)
        })
    )
)
