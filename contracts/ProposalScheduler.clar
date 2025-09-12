;; title: ProposalScheduler
;; version: 1.0
;; summary: Advanced proposal scheduling system for coordinated governance
;; description: Enables time-based proposal activation, queue management, and governance coordination

;; Constants for error handling
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-SCHEDULE-NOT-FOUND (err u201))
(define-constant ERR-INVALID-SCHEDULE-TIME (err u202))
(define-constant ERR-SCHEDULE-ALREADY-ACTIVATED (err u203))
(define-constant ERR-QUEUE-FULL (err u204))
(define-constant ERR-SCHEDULE-LOCKED (err u205))
(define-constant ERR-INVALID-PRIORITY (err u206))
(define-constant ERR-ACTIVATION-FAILED (err u207))

;; Constants for scheduling
(define-constant MAX-QUEUE-SIZE u50)
(define-constant MIN-SCHEDULE-DELAY u144) ;; Approximately 1 day in blocks
(define-constant MAX-SCHEDULE-ADVANCE u20160) ;; Approximately 2 weeks in blocks

;; Data variables
(define-data-var schedule-count uint u0)
(define-data-var active-queue-size uint u0)
(define-data-var scheduler-paused bool false)

;; Main scheduling data structure
(define-map scheduled-proposals
  { schedule-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    activation-block: uint,
    duration: uint,
    min-tokens: uint,
    quorum: uint,
    priority: uint, ;; 1=low, 2=medium, 3=high, 4=critical
    status: (string-ascii 20), ;; "pending", "active", "cancelled", "failed"
    created-at: uint,
    activated-proposal-id: (optional uint),
    category: (optional (string-utf8 20))
  }
)

;; Queue management for activation ordering
(define-map activation-queue
  { position: uint }
  { schedule-id: uint, activation-block: uint, priority: uint }
)

;; Track scheduling permissions
(define-map scheduler-permissions
  { user: principal }
  { 
    can-schedule: bool,
    max-priority: uint,
    granted-by: principal,
    granted-at: uint
  }
)

;; Batch activation tracking
(define-map activation-batches
  { batch-id: uint }
  {
    activator: principal,
    total-scheduled: uint,
    successful: uint,
    failed: uint,
    processed-at: uint
  }
)

(define-data-var batch-count uint u0)

;; Owner permissions
(define-constant CONTRACT-OWNER tx-sender)

;; Private helper functions

;; Check if user can schedule proposals
(define-private (can-user-schedule (user principal))
  (let 
    (
      (permissions (map-get? scheduler-permissions { user: user }))
    )
    (match permissions
      some-perms (get can-schedule some-perms)
      false
    )
  )
)

;; Get maximum priority level for user
(define-private (get-user-max-priority (user principal))
  (let 
    (
      (permissions (map-get? scheduler-permissions { user: user }))
    )
    (match permissions
      some-perms (get max-priority some-perms)
      u1
    )
  )
)

;; Insert scheduled proposal into activation queue based on priority and time
(define-private (insert-into-queue (schedule-id uint) (activation-block uint) (priority uint))
  (let 
    (
      (current-size (var-get active-queue-size))
    )
    (if (< current-size MAX-QUEUE-SIZE)
      (begin
        (map-set activation-queue
          { position: current-size }
          { schedule-id: schedule-id, activation-block: activation-block, priority: priority }
        )
        (var-set active-queue-size (+ current-size u1))
        (ok true)
      )
      (err ERR-QUEUE-FULL)
    )
  )
)

;; Remove from queue and reorganize
(define-private (remove-from-queue (target-schedule-id uint))
  (let 
    (
      (queue-size (var-get active-queue-size))
    )
    (if (> queue-size u0)
      (begin
        (var-set active-queue-size (- queue-size u1))
        (ok true)
      )
      (ok false)
    )
  )
)

;; Public functions

;; Grant scheduling permissions to a user
(define-public (grant-scheduler-permissions (user principal) (max-priority uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= max-priority u4) ERR-INVALID-PRIORITY)
    
    (map-set scheduler-permissions
      { user: user }
      {
        can-schedule: true,
        max-priority: max-priority,
        granted-by: tx-sender,
        granted-at: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Revoke scheduling permissions
(define-public (revoke-scheduler-permissions (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete scheduler-permissions { user: user })
    (ok true)
  )
)

;; Schedule a new proposal for future activation
(define-public (schedule-proposal 
  (title (string-utf8 100))
  (description (string-utf8 500))
  (activation-block uint)
  (duration uint)
  (min-tokens uint)
  (quorum uint)
  (priority uint)
  (category (optional (string-utf8 20))))
  (let 
    (
      (schedule-id (var-get schedule-count))
      (current-block stacks-block-height)
      (user-max-priority (get-user-max-priority tx-sender))
    )
    (asserts! (not (var-get scheduler-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (can-user-schedule tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (>= activation-block (+ current-block MIN-SCHEDULE-DELAY)) ERR-INVALID-SCHEDULE-TIME)
    (asserts! (<= activation-block (+ current-block MAX-SCHEDULE-ADVANCE)) ERR-INVALID-SCHEDULE-TIME)
    (asserts! (<= priority user-max-priority) ERR-INVALID-PRIORITY)
    
    ;; Store the scheduled proposal
    (map-set scheduled-proposals
      { schedule-id: schedule-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        activation-block: activation-block,
        duration: duration,
        min-tokens: min-tokens,
        quorum: quorum,
        priority: priority,
        status: "pending",
        created-at: current-block,
        activated-proposal-id: none,
        category: category
      }
    )
    
    ;; Add to activation queue
    (unwrap! (insert-into-queue schedule-id activation-block priority) ERR-QUEUE-FULL)
    
    ;; Increment counter
    (var-set schedule-count (+ schedule-id u1))
    
    (ok schedule-id)
  )
)

;; Cancel a scheduled proposal (only by creator or owner)
(define-public (cancel-scheduled-proposal (schedule-id uint))
  (let 
    (
      (scheduled (unwrap! (map-get? scheduled-proposals { schedule-id: schedule-id }) ERR-SCHEDULE-NOT-FOUND))
    )
    (asserts! (or 
                (is-eq tx-sender (get creator scheduled))
                (is-eq tx-sender CONTRACT-OWNER)
              ) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status scheduled) "pending") ERR-SCHEDULE-ALREADY-ACTIVATED)
    
    ;; Update status
    (map-set scheduled-proposals
      { schedule-id: schedule-id }
      (merge scheduled { status: "cancelled" })
    )
    
    ;; Remove from queue
    (unwrap! (remove-from-queue schedule-id) ERR-SCHEDULE-NOT-FOUND)
    
    (ok true)
  )
)

;; Activate ready proposals (can be called by anyone)
(define-public (activate-ready-proposals)
  (let 
    (
      (current-block stacks-block-height)
      (batch-id (var-get batch-count))
    )
    (asserts! (not (var-get scheduler-paused)) ERR-NOT-AUTHORIZED)
    
    ;; Process activation queue - simplified version for demonstration
    (let 
      (
        (processed-count (process-activation-queue current-block))
      )
      ;; Record batch activation
      (map-set activation-batches
        { batch-id: batch-id }
        {
          activator: tx-sender,
          total-scheduled: processed-count,
          successful: processed-count, ;; Simplified - assume all succeed
          failed: u0,
          processed-at: current-block
        }
      )
      
      (var-set batch-count (+ batch-id u1))
      (ok processed-count)
    )
  )
)

;; Simplified queue processing helper
(define-private (process-activation-queue (current-block uint))
  (let 
    (
      (queue-size (var-get active-queue-size))
    )
    ;; In a real implementation, this would iterate through the queue
    ;; For now, return a placeholder count
    (if (> queue-size u0) u1 u0)
  )
)

;; Update scheduled proposal details (before activation)
(define-public (update-scheduled-proposal 
  (schedule-id uint)
  (new-title (optional (string-utf8 100)))
  (new-description (optional (string-utf8 500)))
  (new-category (optional (string-utf8 20))))
  (let 
    (
      (scheduled (unwrap! (map-get? scheduled-proposals { schedule-id: schedule-id }) ERR-SCHEDULE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get creator scheduled)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status scheduled) "pending") ERR-SCHEDULE-LOCKED)
    
    (map-set scheduled-proposals
      { schedule-id: schedule-id }
      (merge scheduled
        {
          title: (default-to (get title scheduled) new-title),
          description: (default-to (get description scheduled) new-description),
          category: (if (is-some new-category) new-category (get category scheduled))
        }
      )
    )
    (ok true)
  )
)

;; Toggle scheduler pause (owner only)
(define-public (toggle-scheduler-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set scheduler-paused (not (var-get scheduler-paused))))
  )
)

;; Read-only functions

;; Get scheduled proposal details
(define-read-only (get-scheduled-proposal (schedule-id uint))
  (map-get? scheduled-proposals { schedule-id: schedule-id })
)

;; Get user scheduling permissions
(define-read-only (get-scheduler-permissions (user principal))
  (map-get? scheduler-permissions { user: user })
)

;; Get queue position details
(define-read-only (get-queue-position (position uint))
  (map-get? activation-queue { position: position })
)

;; Get activation batch details
(define-read-only (get-activation-batch (batch-id uint))
  (map-get? activation-batches { batch-id: batch-id })
)

;; Get scheduler statistics
(define-read-only (get-scheduler-stats)
  (ok {
    total-scheduled: (var-get schedule-count),
    queue-size: (var-get active-queue-size),
    max-queue-size: MAX-QUEUE-SIZE,
    is-paused: (var-get scheduler-paused),
    min-delay-blocks: MIN-SCHEDULE-DELAY,
    max-advance-blocks: MAX-SCHEDULE-ADVANCE
  })
)

;; Check if proposal can be activated
(define-read-only (can-activate-proposal (schedule-id uint))
  (let 
    (
      (scheduled (map-get? scheduled-proposals { schedule-id: schedule-id }))
      (current-block stacks-block-height)
    )
    (match scheduled
      some-scheduled
        (and 
          (is-eq (get status some-scheduled) "pending")
          (>= current-block (get activation-block some-scheduled))
          (not (var-get scheduler-paused))
        )
      false
    )
  )
)

;; Get proposals ready for activation
(define-read-only (get-ready-proposals-count)
  (let 
    (
      (current-block stacks-block-height)
      (queue-size (var-get active-queue-size))
    )
    ;; Simplified implementation - would normally iterate through queue
    (ok (if (> queue-size u0) u1 u0))
  )
)



