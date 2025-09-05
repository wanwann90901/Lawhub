;; title: Lawhub
;; version: 1.0.0
;; summary: Decentralized policy proposal and voting platform
;; description: Citizens can submit policy proposals and vote on them democratically

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_VOTING_NOT_ENDED (err u104))
(define-constant ERR_INSUFFICIENT_STAKE (err u105))
(define-constant ERR_PROPOSAL_EXECUTED (err u106))
(define-constant ERR_AMENDMENT_NOT_FOUND (err u107))
(define-constant ERR_AMENDMENT_ALREADY_APPLIED (err u108))
(define-constant ERR_INVALID_AMENDMENT_PROPOSER (err u109))
(define-constant ERR_AMENDMENT_NOT_APPROVED (err u110))
(define-constant ERR_AMENDMENT_VOTING_ENDED (err u111))
(define-constant ERR_INVALID_CATEGORY (err u112))
(define-constant ERR_EXPERTISE_NOT_FOUND (err u113))
(define-constant ERR_INSUFFICIENT_EXPERTISE (err u114))
(define-constant ERR_CATEGORY_ALREADY_EXISTS (err u115))

(define-constant PROPOSAL_STAKE u1000000)
(define-constant VOTING_PERIOD u1440)
(define-constant MIN_VOTES_FOR_EXECUTION u10)
(define-constant AMENDMENT_STAKE u500000)
(define-constant AMENDMENT_APPROVAL_THRESHOLD u5)
(define-constant AMENDMENT_VOTING_PERIOD u720)
(define-constant MIN_EXPERTISE_FOR_CATEGORY u50)
(define-constant EXPERTISE_VOTE_MULTIPLIER u2)
(define-constant ERR_MILESTONE_NOT_FOUND (err u116))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u117))
(define-constant ERR_NOT_MILESTONE_PROPOSER (err u118))
(define-constant ERR_PROPOSAL_NOT_EXECUTED (err u119))
(define-constant ERR_MILESTONE_DEADLINE_PASSED (err u120))
(define-constant MAX_MILESTONES_PER_PROPOSAL u10)

(define-data-var proposal-counter uint u0)
(define-data-var milestone-counter uint u0)
(define-data-var category-counter uint u0)
(define-data-var total-citizens uint u0)
(define-data-var amendment-counter uint u0)

(define-map proposals
  uint
  {
    id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    created-at: uint,
    voting-ends-at: uint,
    yes-votes: uint,
    no-votes: uint,
    total-votes: uint,
    executed: bool,
    stake: uint,
    category: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-map citizen-registry
  principal
  {
    registered: bool,
    reputation: uint,
    proposals-created: uint,
    votes-cast: uint,
    registration-block: uint
  }
)

(define-map citizen-stakes
  principal
  uint
)

(define-map amendments
  uint
  {
    id: uint,
    proposal-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amendment-text: (string-ascii 500),
    proposer: principal,
    created-at: uint,
    voting-ends-at: uint,
    approval-votes: uint,
    rejection-votes: uint,
    total-votes: uint,
    approved: bool,
    applied: bool,
    stake: uint
  }
)

(define-map amendment-votes
  { amendment-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-map proposal-amendments
  uint
  (list 50 uint)
)

(define-map proposal-categories
  uint
  {
    id: uint,
    name: (string-ascii 50),
    description: (string-ascii 200),
    created-by: principal,
    created-at: uint,
    active: bool
  }
)

(define-map citizen-expertise
  { citizen: principal, category: uint }
  {
    level: uint,
    successful-votes: uint,
    total-votes: uint,
    last-updated: uint
  }
)

(define-map category-experts
  uint
  (list 100 principal)
)

;; Proposal milestone system maps
(define-map proposal-milestones
  uint
  {
    id: uint,
    proposal-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    deadline: uint,
    completed: bool,
    completion-date: (optional uint),
    completed-by: (optional principal),
    created-at: uint,
    order: uint
  }
)

(define-map proposal-milestone-list
  uint
  (list 10 uint)
)

(define-public (register-citizen)
  (let ((caller tx-sender))
    (match (map-get? citizen-registry caller)
      existing-citizen ERR_NOT_AUTHORIZED
      (begin
        (map-set citizen-registry caller {
          registered: true,
          reputation: u100,
          proposals-created: u0,
          votes-cast: u0,
          registration-block: stacks-block-height
        })
        (var-set total-citizens (+ (var-get total-citizens) u1))
        (ok true)
      )
    )
  )
)

(define-public (submit-proposal (title (string-ascii 100)) (description (string-ascii 500)) (category uint))
  (let (
    (caller tx-sender)
    (proposal-id (+ (var-get proposal-counter) u1))
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? proposal-categories category)
          category-data
          (if (get active category-data)
            (begin
              (try! (stx-transfer? PROPOSAL_STAKE caller (as-contract tx-sender)))
              (map-set proposals proposal-id {
                id: proposal-id,
                title: title,
                description: description,
                proposer: caller,
                created-at: current-block,
                voting-ends-at: (+ current-block VOTING_PERIOD),
                yes-votes: u0,
                no-votes: u0,
                total-votes: u0,
                executed: false,
                stake: PROPOSAL_STAKE,
                category: category
              })
              (map-set citizen-registry caller
                (merge citizen-data { proposals-created: (+ (get proposals-created citizen-data) u1) })
              )
              (var-set proposal-counter proposal-id)
              (ok proposal-id)
            )
            ERR_INVALID_CATEGORY
          )
          ERR_INVALID_CATEGORY
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? proposals proposal-id)
          proposal-data
          (if (< current-block (get voting-ends-at proposal-data))
            (match (map-get? votes { proposal-id: proposal-id, voter: caller })
              existing-vote ERR_ALREADY_VOTED
              (let (
                (vote-weight (calculate-vote-weight caller))
                (updated-yes-votes (if vote-yes 
                  (+ (get yes-votes proposal-data) vote-weight) 
                  (get yes-votes proposal-data)))
                (updated-no-votes (if vote-yes 
                  (get no-votes proposal-data) 
                  (+ (get no-votes proposal-data) vote-weight)))
              )
                (map-set votes { proposal-id: proposal-id, voter: caller } 
                  { vote: vote-yes, weight: vote-weight })
                (map-set proposals proposal-id
                  (merge proposal-data {
                    yes-votes: updated-yes-votes,
                    no-votes: updated-no-votes,
                    total-votes: (+ (get total-votes proposal-data) vote-weight)
                  })
                )
                (map-set citizen-registry caller
                  (merge citizen-data { 
                    votes-cast: (+ (get votes-cast citizen-data) u1),
                    reputation: (+ (get reputation citizen-data) u1)
                  })
                )
                (ok true)
              )
            )
            ERR_VOTING_ENDED
          )
          ERR_PROPOSAL_NOT_FOUND
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let ((current-block stacks-block-height))
    (match (map-get? proposals proposal-id)
      proposal-data
      (if (and 
        (>= current-block (get voting-ends-at proposal-data))
        (not (get executed proposal-data))
        (>= (get total-votes proposal-data) MIN_VOTES_FOR_EXECUTION)
        (> (get yes-votes proposal-data) (get no-votes proposal-data))
      )
        (begin
          (map-set proposals proposal-id
            (merge proposal-data { executed: true })
          )
          (match (map-get? citizen-registry (get proposer proposal-data))
            proposer-data
            (begin
              (map-set citizen-registry (get proposer proposal-data)
                (merge proposer-data { reputation: (+ (get reputation proposer-data) u50) })
              )
              (try! (as-contract (stx-transfer? (get stake proposal-data) tx-sender (get proposer proposal-data))))
              (ok true)
            )
            (ok true)
          )
        )
        (if (>= current-block (get voting-ends-at proposal-data))
          (begin
            (map-set proposals proposal-id
              (merge proposal-data { executed: true })
            )
            (ok false)
          )
          ERR_VOTING_NOT_ENDED
        )
      )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

(define-public (delegate-vote (proposal-id uint) (delegate principal))
  (let ((caller tx-sender))
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? citizen-registry delegate)
          delegate-data
          (if (get registered delegate-data)
            (begin
              (map-set citizen-stakes delegate 
                (+ (default-to u0 (map-get? citizen-stakes delegate)) u1))
              (ok true)
            )
            ERR_NOT_AUTHORIZED
          )
          ERR_NOT_AUTHORIZED
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (submit-amendment (proposal-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (amendment-text (string-ascii 500)))
  (let (
    (caller tx-sender)
    (amendment-id (+ (var-get amendment-counter) u1))
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? proposals proposal-id)
          proposal-data
          (if (and 
            (< current-block (get voting-ends-at proposal-data))
            (not (get executed proposal-data))
          )
            (begin
              (try! (stx-transfer? AMENDMENT_STAKE caller (as-contract tx-sender)))
              (map-set amendments amendment-id {
                id: amendment-id,
                proposal-id: proposal-id,
                title: title,
                description: description,
                amendment-text: amendment-text,
                proposer: caller,
                created-at: current-block,
                voting-ends-at: (+ current-block AMENDMENT_VOTING_PERIOD),
                approval-votes: u0,
                rejection-votes: u0,
                total-votes: u0,
                approved: false,
                applied: false,
                stake: AMENDMENT_STAKE
              })
              (map-set proposal-amendments proposal-id
                (unwrap-panic (as-max-len? 
                  (append (default-to (list) (map-get? proposal-amendments proposal-id)) amendment-id)
                  u50
                ))
              )
              (var-set amendment-counter amendment-id)
              (ok amendment-id)
            )
            ERR_VOTING_ENDED
          )
          ERR_PROPOSAL_NOT_FOUND
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (vote-on-amendment (amendment-id uint) (support bool))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? amendments amendment-id)
          amendment-data
          (if (< current-block (get voting-ends-at amendment-data))
            (match (map-get? amendment-votes { amendment-id: amendment-id, voter: caller })
              existing-vote ERR_ALREADY_VOTED
              (let (
                (vote-weight (calculate-vote-weight caller))
                (updated-approval-votes (if support 
                  (+ (get approval-votes amendment-data) vote-weight) 
                  (get approval-votes amendment-data)))
                (updated-rejection-votes (if support 
                  (get rejection-votes amendment-data) 
                  (+ (get rejection-votes amendment-data) vote-weight)))
              )
                (map-set amendment-votes { amendment-id: amendment-id, voter: caller } 
                  { vote: support, weight: vote-weight })
                (map-set amendments amendment-id
                  (merge amendment-data {
                    approval-votes: updated-approval-votes,
                    rejection-votes: updated-rejection-votes,
                    total-votes: (+ (get total-votes amendment-data) vote-weight)
                  })
                )
                (map-set citizen-registry caller
                  (merge citizen-data { 
                    votes-cast: (+ (get votes-cast citizen-data) u1),
                    reputation: (+ (get reputation citizen-data) u1)
                  })
                )
                (ok true)
              )
            )
            ERR_AMENDMENT_VOTING_ENDED
          )
          ERR_AMENDMENT_NOT_FOUND
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (finalize-amendment (amendment-id uint))
  (let ((current-block stacks-block-height))
    (match (map-get? amendments amendment-id)
      amendment-data
      (if (>= current-block (get voting-ends-at amendment-data))
        (let (
          (approved (and 
            (>= (get total-votes amendment-data) AMENDMENT_APPROVAL_THRESHOLD)
            (> (get approval-votes amendment-data) (get rejection-votes amendment-data))
          ))
        )
          (map-set amendments amendment-id
            (merge amendment-data { approved: approved })
          )
          (match (map-get? citizen-registry (get proposer amendment-data))
            proposer-data
            (if approved
              (begin
                (map-set citizen-registry (get proposer amendment-data)
                  (merge proposer-data { reputation: (+ (get reputation proposer-data) u25) })
                )
                (try! (as-contract (stx-transfer? (get stake amendment-data) tx-sender (get proposer amendment-data))))
                (ok true)
              )
              (ok false)
            )
            (ok approved)
          )
        )
        ERR_AMENDMENT_VOTING_ENDED
      )
      ERR_AMENDMENT_NOT_FOUND
    )
  )
)

(define-public (apply-amendment (amendment-id uint))
  (let ((caller tx-sender))
    (match (map-get? amendments amendment-id)
      amendment-data
      (if (and 
        (get approved amendment-data)
        (not (get applied amendment-data))
      )
        (match (map-get? proposals (get proposal-id amendment-data))
          proposal-data
          (if (or 
            (is-eq caller (get proposer proposal-data))
            (is-eq caller CONTRACT_OWNER)
          )
            (begin
              (map-set amendments amendment-id
                (merge amendment-data { applied: true })
              )
              (match (map-get? citizen-registry (get proposer amendment-data))
                proposer-data
                (begin
                  (map-set citizen-registry (get proposer amendment-data)
                    (merge proposer-data { reputation: (+ (get reputation proposer-data) u10) })
                  )
                  (ok true)
                )
                (ok true)
              )
            )
            ERR_INVALID_AMENDMENT_PROPOSER
          )
          ERR_PROPOSAL_NOT_FOUND
        )
        (if (get applied amendment-data)
          ERR_AMENDMENT_ALREADY_APPLIED
          ERR_AMENDMENT_NOT_APPROVED
        )
      )
      ERR_AMENDMENT_NOT_FOUND
    )
  )
)

(define-public (create-category (name (string-ascii 50)) (description (string-ascii 200)))
  (let (
    (caller tx-sender)
    (category-id (+ (var-get category-counter) u1))
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (and 
        (get registered citizen-data)
        (>= (get reputation citizen-data) MIN_EXPERTISE_FOR_CATEGORY)
      )
        (begin
          (map-set proposal-categories category-id {
            id: category-id,
            name: name,
            description: description,
            created-by: caller,
            created-at: current-block,
            active: true
          })
          (var-set category-counter category-id)
          (ok category-id)
        )
        ERR_INSUFFICIENT_EXPERTISE
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (declare-expertise (category uint))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? proposal-categories category)
          category-data
          (if (get active category-data)
            (match (map-get? citizen-expertise { citizen: caller, category: category })
              existing-expertise ERR_NOT_AUTHORIZED
              (begin
                (map-set citizen-expertise { citizen: caller, category: category } {
                  level: u1,
                  successful-votes: u0,
                  total-votes: u0,
                  last-updated: current-block
                })
                (map-set category-experts category
                  (unwrap-panic (as-max-len? 
                    (append (default-to (list) (map-get? category-experts category)) caller)
                    u100
                  ))
                )
                (ok true)
              )
            )
            ERR_INVALID_CATEGORY
          )
          ERR_INVALID_CATEGORY
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-public (vote-on-proposal-with-expertise (proposal-id uint) (vote-yes bool))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
        (match (map-get? proposals proposal-id)
          proposal-data
          (if (< current-block (get voting-ends-at proposal-data))
            (match (map-get? votes { proposal-id: proposal-id, voter: caller })
              existing-vote ERR_ALREADY_VOTED
              (let (
                (category (get category proposal-data))
                (base-weight (calculate-vote-weight caller))
                (expertise-multiplier (get-expertise-multiplier caller category))
                (final-weight (* base-weight expertise-multiplier))
                (updated-yes-votes (if vote-yes 
                  (+ (get yes-votes proposal-data) final-weight) 
                  (get yes-votes proposal-data)))
                (updated-no-votes (if vote-yes 
                  (get no-votes proposal-data) 
                  (+ (get no-votes proposal-data) final-weight)))
              )
                (map-set votes { proposal-id: proposal-id, voter: caller } 
                  { vote: vote-yes, weight: final-weight })
                (map-set proposals proposal-id
                  (merge proposal-data {
                    yes-votes: updated-yes-votes,
                    no-votes: updated-no-votes,
                    total-votes: (+ (get total-votes proposal-data) final-weight)
                  })
                )
                (map-set citizen-registry caller
                  (merge citizen-data { 
                    votes-cast: (+ (get votes-cast citizen-data) u1),
                    reputation: (+ (get reputation citizen-data) u1)
                  })
                )
                (update-expertise-after-vote caller category)
                (ok true)
              )
            )
            ERR_VOTING_ENDED
          )
          ERR_PROPOSAL_NOT_FOUND
        )
        ERR_NOT_AUTHORIZED
      )
      ERR_NOT_AUTHORIZED
    )
  )
)

(define-private (get-expertise-multiplier (citizen principal) (category uint))
  (match (map-get? citizen-expertise { citizen: citizen, category: category })
    expertise-data
    (if (>= (get level expertise-data) u1)
      EXPERTISE_VOTE_MULTIPLIER
      u1
    )
    u1
  )
)

(define-private (update-expertise-after-vote (citizen principal) (category uint))
  (match (map-get? citizen-expertise { citizen: citizen, category: category })
    expertise-data
    (map-set citizen-expertise { citizen: citizen, category: category }
      (merge expertise-data { 
        total-votes: (+ (get total-votes expertise-data) u1),
        last-updated: stacks-block-height
      })
    )
    true
  )
)

(define-public (update-expertise-on-proposal-success (proposal-id uint))
  (let ((current-block stacks-block-height))
    (match (map-get? proposals proposal-id)
      proposal-data
      (if (and 
        (>= current-block (get voting-ends-at proposal-data))
        (get executed proposal-data)
        (> (get yes-votes proposal-data) (get no-votes proposal-data))
      )
        (let ((category (get category proposal-data)))
          (update-all-expertise-for-category category proposal-id)
          (ok true)
        )
        ERR_VOTING_NOT_ENDED
      )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

(define-private (update-all-expertise-for-category (category uint) (proposal-id uint))
  (match (map-get? category-experts category)
    experts-list
    (fold update-single-expert-for-proposal experts-list true)
    true
  )
)

(define-private (update-single-expert-for-proposal (expert principal) (acc bool))
  (begin
    (update-expert-expertise-for-current-proposal expert)
    acc
  )
)

(define-private (update-expert-expertise-for-current-proposal (expert principal))
  true
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-citizen-info (citizen principal))
  (map-get? citizen-registry citizen)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (get-total-citizens)
  (var-get total-citizens)
)

(define-read-only (is-voting-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal-data
    (< stacks-block-height (get voting-ends-at proposal-data))
    false
  )
)

(define-read-only (get-proposal-result (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal-data
    (if (>= stacks-block-height (get voting-ends-at proposal-data))
      (some {
        passed: (> (get yes-votes proposal-data) (get no-votes proposal-data)),
        yes-votes: (get yes-votes proposal-data),
        no-votes: (get no-votes proposal-data),
        total-votes: (get total-votes proposal-data),
        executed: (get executed proposal-data)
      })
      none
    )
    none
  )
)

(define-private (calculate-vote-weight (voter principal))
  (match (map-get? citizen-registry voter)
    citizen-data
    (let (
      (base-weight u1)
      (reputation-bonus (/ (get reputation citizen-data) u100))
      (stake-bonus (default-to u0 (map-get? citizen-stakes voter)))
    )
      (+ base-weight reputation-bonus stake-bonus)
    )
    u1
  )
)

(define-read-only (get-citizen-vote-weight (citizen principal))
  (calculate-vote-weight citizen)
)

(define-read-only (get-category (category-id uint))
  (map-get? proposal-categories category-id)
)

(define-read-only (get-citizen-expertise (citizen principal) (category uint))
  (map-get? citizen-expertise { citizen: citizen, category: category })
)

(define-read-only (get-category-experts (category uint))
  (default-to (list) (map-get? category-experts category))
)

(define-read-only (get-category-count)
  (var-get category-counter)
)

(define-read-only (get-proposals-by-category (category uint))
  (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
)

(define-private (is-proposal-in-category (proposal-id uint) (target-category uint))
  (match (map-get? proposals proposal-id)
    proposal-data
    (is-eq (get category proposal-data) target-category)
    false
  )
)

(define-read-only (get-citizen-expertise-level (citizen principal) (category uint))
  (match (map-get? citizen-expertise { citizen: citizen, category: category })
    expertise-data
    (get level expertise-data)
    u0
  )
)

(define-read-only (get-expertise-vote-weight (citizen principal) (category uint))
  (let (
    (base-weight (calculate-vote-weight citizen))
    (multiplier (get-expertise-multiplier citizen category))
  )
    (* base-weight multiplier)
  )
)

(define-read-only (get-active-categories)
  (filter is-category-active (list 
    u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
  ))
)

(define-private (is-category-active (category-id uint))
  (match (map-get? proposal-categories category-id)
    category-data
    (get active category-data)
    false
  )
)

(define-read-only (get-amendment (amendment-id uint))
  (map-get? amendments amendment-id)
)

(define-read-only (get-amendment-vote (amendment-id uint) (voter principal))
  (map-get? amendment-votes { amendment-id: amendment-id, voter: voter })
)

(define-read-only (get-proposal-amendments (proposal-id uint))
  (default-to (list) (map-get? proposal-amendments proposal-id))
)

(define-read-only (get-amendment-count)
  (var-get amendment-counter)
)

(define-read-only (is-amendment-voting-active (amendment-id uint))
  (match (map-get? amendments amendment-id)
    amendment-data
    (< stacks-block-height (get voting-ends-at amendment-data))
    false
  )
)

(define-read-only (get-amendment-result (amendment-id uint))
  (match (map-get? amendments amendment-id)
    amendment-data
    (if (>= stacks-block-height (get voting-ends-at amendment-data))
      (some {
        approved: (and 
          (>= (get total-votes amendment-data) AMENDMENT_APPROVAL_THRESHOLD)
          (> (get approval-votes amendment-data) (get rejection-votes amendment-data))
        ),
        approval-votes: (get approval-votes amendment-data),
        rejection-votes: (get rejection-votes amendment-data),
        total-votes: (get total-votes amendment-data),
        applied: (get applied amendment-data)
      })
      none
    )
    none
  )
)

(define-read-only (get-active-amendments)
  (let ((current-block stacks-block-height))
    (filter is-amendment-active (list 
      u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
      u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
    ))
  )
)

(define-private (is-amendment-active (amendment-id uint))
  (match (map-get? amendments amendment-id)
    amendment-data
    (< stacks-block-height (get voting-ends-at amendment-data))
    false
  )
)

(define-read-only (get-active-proposals)
  (let ((current-block stacks-block-height))
    (filter is-proposal-active (list 
      u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
    ))
  )
)

(define-private (is-proposal-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal-data
    (< stacks-block-height (get voting-ends-at proposal-data))
    false
  )
)

;; Milestone Management Functions

;; Create milestone for executed proposal
(define-public (create-milestone 
  (proposal-id uint) 
  (title (string-ascii 100)) 
  (description (string-ascii 300)) 
  (deadline uint)
  (order uint))
  (let (
    (caller tx-sender)
    (milestone-id (+ (var-get milestone-counter) u1))
    (current-block stacks-block-height)
  )
    (match (map-get? proposals proposal-id)
      proposal-data
      (if (and 
        (get executed proposal-data)
        (or (is-eq caller (get proposer proposal-data)) 
            (is-eq caller CONTRACT_OWNER))
        (> deadline current-block)
      )
        (let (
          (current-milestones (default-to (list) (map-get? proposal-milestone-list proposal-id)))
          (milestone-count (len current-milestones))
        )
          (if (< milestone-count MAX_MILESTONES_PER_PROPOSAL)
            (begin
              (map-set proposal-milestones milestone-id {
                id: milestone-id,
                proposal-id: proposal-id,
                title: title,
                description: description,
                deadline: deadline,
                completed: false,
                completion-date: none,
                completed-by: none,
                created-at: current-block,
                order: order
              })
              (map-set proposal-milestone-list proposal-id
                (unwrap-panic (as-max-len? (append current-milestones milestone-id) u10))
              )
              (var-set milestone-counter milestone-id)
              (ok milestone-id)
            )
            ERR_NOT_AUTHORIZED
          )
        )
        (if (not (get executed proposal-data))
          ERR_PROPOSAL_NOT_EXECUTED
          (if (<= deadline current-block)
            ERR_MILESTONE_DEADLINE_PASSED
            ERR_NOT_MILESTONE_PROPOSER
          )
        )
      )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

;; Mark milestone as completed
(define-public (complete-milestone (milestone-id uint))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    (match (map-get? proposal-milestones milestone-id)
      milestone-data
      (if (not (get completed milestone-data))
        (match (map-get? proposals (get proposal-id milestone-data))
          proposal-data
          (if (or 
            (is-eq caller (get proposer proposal-data))
            (is-eq caller CONTRACT_OWNER)
          )
            (if (<= current-block (get deadline milestone-data))
              (begin
                (map-set proposal-milestones milestone-id
                  (merge milestone-data {
                    completed: true,
                    completion-date: (some current-block),
                    completed-by: (some caller)
                  })
                )
                ;; Award reputation bonus for completing milestone on time
                (match (map-get? citizen-registry caller)
                  citizen-data
                  (map-set citizen-registry caller
                    (merge citizen-data { 
                      reputation: (+ (get reputation citizen-data) u10) 
                    })
                  )
                  true
                )
                (ok true)
              )
              (begin
                ;; Still allow completion but with reduced reputation
                (map-set proposal-milestones milestone-id
                  (merge milestone-data {
                    completed: true,
                    completion-date: (some current-block),
                    completed-by: (some caller)
                  })
                )
                (ok true)
              )
            )
            ERR_NOT_MILESTONE_PROPOSER
          )
          ERR_PROPOSAL_NOT_FOUND
        )
        ERR_MILESTONE_ALREADY_COMPLETED
      )
      ERR_MILESTONE_NOT_FOUND
    )
  )
)

;; Milestone Query Functions

;; Get milestone details
(define-read-only (get-milestone (milestone-id uint))
  (map-get? proposal-milestones milestone-id)
)

;; Get all milestones for a proposal
(define-read-only (get-proposal-milestones (proposal-id uint))
  (default-to (list) (map-get? proposal-milestone-list proposal-id))
)

;; Get milestone count
(define-read-only (get-milestone-count)
  (var-get milestone-counter)
)

;; Calculate proposal progress percentage
(define-read-only (get-proposal-progress (proposal-id uint))
  (let (
    (milestones (default-to (list) (map-get? proposal-milestone-list proposal-id)))
    (total-milestones (len milestones))
  )
    (if (> total-milestones u0)
      (let (
        (completed-count (count-completed-milestones milestones))
        (progress (/ (* completed-count u100) total-milestones))
      )
        (some { 
          completed: completed-count, 
          total: total-milestones, 
          percentage: progress 
        })
      )
      none
    )
  )
)

;; Check if milestone is overdue
(define-read-only (is-milestone-overdue (milestone-id uint))
  (match (map-get? proposal-milestones milestone-id)
    milestone-data
    (and 
      (not (get completed milestone-data))
      (> stacks-block-height (get deadline milestone-data))
    )
    false
  )
)

;; Get overdue milestones for a proposal
(define-read-only (get-overdue-milestones (proposal-id uint))
  (let (
    (milestones (default-to (list) (map-get? proposal-milestone-list proposal-id)))
  )
    (filter-overdue-milestones milestones)
  )
)

;; Private helper functions
(define-private (count-completed-milestones (milestones (list 10 uint)))
  (fold check-milestone-completion milestones u0)
)

(define-private (check-milestone-completion (milestone-id uint) (count uint))
  (match (map-get? proposal-milestones milestone-id)
    milestone-data
    (if (get completed milestone-data)
      (+ count u1)
      count
    )
    count
  )
)

(define-private (filter-overdue-milestones (milestones (list 10 uint)))
  (filter is-milestone-overdue milestones)
)



