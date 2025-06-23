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

(define-constant PROPOSAL_STAKE u1000000)
(define-constant VOTING_PERIOD u1440)
(define-constant MIN_VOTES_FOR_EXECUTION u10)

(define-data-var proposal-counter uint u0)
(define-data-var total-citizens uint u0)

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
    stake: uint
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

(define-public (submit-proposal (title (string-ascii 100)) (description (string-ascii 500)))
  (let (
    (caller tx-sender)
    (proposal-id (+ (var-get proposal-counter) u1))
    (current-block stacks-block-height)
  )
    (match (map-get? citizen-registry caller)
      citizen-data
      (if (get registered citizen-data)
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
            stake: PROPOSAL_STAKE
          })
          (map-set citizen-registry caller
            (merge citizen-data { proposals-created: (+ (get proposals-created citizen-data) u1) })
          )
          (var-set proposal-counter proposal-id)
          (ok proposal-id)
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