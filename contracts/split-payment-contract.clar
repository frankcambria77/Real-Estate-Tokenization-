(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-not-authorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-property-not-active (err u106))
(define-constant err-insufficient-tokens (err u107))
(define-constant err-invalid-valuation (err u108))
(define-constant err-valuation-change-too-large (err u109))
(define-constant err-rental-period-not-found (err u110))
(define-constant err-already-claimed (err u111))
(define-constant err-no-rental-income (err u112))
(define-constant err-offer-not-found (err u113))
(define-constant err-offer-expired (err u114))
(define-constant err-invalid-price (err u115))
(define-constant err-offer-already-exists (err u116))

(define-map properties
  { property-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    total-value: uint,
    total-tokens: uint,
    available-tokens: uint,
    price-per-token: uint,
    is-active: bool,
    is-listed: bool
  }
)

(define-map property-tokens
  { property-id: uint, holder: principal }
  { amount: uint }
)

(define-map user-properties
  { user: principal, property-id: uint }
  { tokens-owned: uint }
)

(define-data-var next-property-id uint u1)

(define-map property-valuations
  { property-id: uint, valuation-id: uint }
  {
    new-value: uint,
    old-value: uint,
    valuation-date: uint,
    updated-by: principal
  }
)

(define-map property-valuation-count
  { property-id: uint }
  { count: uint }
)

(define-map rental-periods
  { property-id: uint, period-id: uint }
  {
    rental-amount: uint,
    period-start: uint,
    period-end: uint,
    total-tokens-snapshot: uint,
    distributed: bool
  }
)

(define-map rental-period-count
  { property-id: uint }
  { count: uint }
)

(define-map rental-claims
  { property-id: uint, period-id: uint, holder: principal }
  { 
    amount-claimed: uint,
    claimed: bool
  }
)

(define-map user-rental-balance
  { property-id: uint, holder: principal }
  { unclaimed-amount: uint }
)

(define-map sell-offers
  { property-id: uint, seller: principal, offer-id: uint }
  {
    token-amount: uint,
    price-per-token: uint,
    expiry-block: uint,
    is-active: bool
  }
)

(define-map buy-offers
  { property-id: uint, buyer: principal, offer-id: uint }
  {
    token-amount: uint,
    price-per-token: uint,
    expiry-block: uint,
    is-active: bool
  }
)

(define-map user-sell-offer-count
  { property-id: uint, seller: principal }
  { count: uint }
)

(define-map user-buy-offer-count
  { property-id: uint, buyer: principal }
  { count: uint }
)

(define-public (create-property (name (string-ascii 50)) (total-value uint) (total-tokens uint))
  (let
    (
      (property-id (var-get next-property-id))
      (price-per-token (/ total-value total-tokens))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> total-value u0) err-invalid-amount)
    (asserts! (> total-tokens u0) err-invalid-amount)
    (map-set properties
      { property-id: property-id }
      {
        owner: tx-sender,
        name: name,
        total-value: total-value,
        total-tokens: total-tokens,
        available-tokens: total-tokens,
        price-per-token: price-per-token,
        is-active: true,
        is-listed: true
      }
    )
    (map-set property-valuation-count
      { property-id: property-id }
      { count: u0 }
    )
    (map-set rental-period-count
      { property-id: property-id }
      { count: u0 }
    )
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (buy-tokens (property-id uint) (token-amount uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
      (total-cost (* (get price-per-token property) token-amount))
      (current-tokens (default-to u0 (get amount (map-get? property-tokens { property-id: property-id, holder: tx-sender }))))
      (current-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: tx-sender, property-id: property-id }))))
    )
    (asserts! (get is-active property) err-property-not-active)
    (asserts! (>= (get available-tokens property) token-amount) err-insufficient-tokens)
    (asserts! (> token-amount u0) err-invalid-amount)
    (try! (stx-transfer? total-cost tx-sender (get owner property)))
    (map-set properties
      { property-id: property-id }
      (merge property { available-tokens: (- (get available-tokens property) token-amount) })
    )
    (map-set property-tokens
      { property-id: property-id, holder: tx-sender }
      { amount: (+ current-tokens token-amount) }
    )
    (map-set user-properties
      { user: tx-sender, property-id: property-id }
      { tokens-owned: (+ current-user-tokens token-amount) }
    )
    (ok token-amount)
  )
)

(define-public (transfer-tokens (property-id uint) (token-amount uint) (recipient principal))
  (let
    (
      (sender-tokens (default-to u0 (get amount (map-get? property-tokens { property-id: property-id, holder: tx-sender }))))
      (recipient-tokens (default-to u0 (get amount (map-get? property-tokens { property-id: property-id, holder: recipient }))))
      (sender-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: tx-sender, property-id: property-id }))))
      (recipient-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: recipient, property-id: property-id }))))
    )
    (asserts! (>= sender-tokens token-amount) err-insufficient-tokens)
    (asserts! (> token-amount u0) err-invalid-amount)
    (map-set property-tokens
      { property-id: property-id, holder: tx-sender }
      { amount: (- sender-tokens token-amount) }
    )
    (map-set property-tokens
      { property-id: property-id, holder: recipient }
      { amount: (+ recipient-tokens token-amount) }
    )
    (map-set user-properties
      { user: tx-sender, property-id: property-id }
      { tokens-owned: (- sender-user-tokens token-amount) }
    )
    (map-set user-properties
      { user: recipient, property-id: property-id }
      { tokens-owned: (+ recipient-user-tokens token-amount) }
    )
    (ok true)
  )
)

(define-public (distribute-dividends (property-id uint) (total-dividend uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner property)) err-not-authorized)
    (asserts! (> total-dividend u0) err-invalid-amount)
    (ok total-dividend)
  )
)

(define-public (claim-dividend (property-id uint) (dividend-per-token uint))
  (let
    (
      (user-tokens (default-to u0 (get amount (map-get? property-tokens { property-id: property-id, holder: tx-sender }))))
      (dividend-amount (* user-tokens dividend-per-token))
    )
    (asserts! (> user-tokens u0) err-insufficient-tokens)
    (asserts! (> dividend-per-token u0) err-invalid-amount)
    (ok dividend-amount)
  )
)

(define-public (deactivate-property (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner property)) err-not-authorized)
    (map-set properties
      { property-id: property-id }
      (merge property { is-active: false })
    )
    (ok true)
  )
)

(define-public (toggle-property-listing (property-id uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner property)) err-not-authorized)
    (map-set properties
      { property-id: property-id }
      (merge property { is-listed: (not (get is-listed property)) })
    )
    (ok (not (get is-listed property)))
  )
)

(define-public (update-property-valuation (property-id uint) (new-value uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
      (current-value (get total-value property))
      (total-tokens (get total-tokens property))
      (new-price-per-token (/ new-value total-tokens))
      (valuation-count (default-to u0 (get count (map-get? property-valuation-count { property-id: property-id }))))
      (next-valuation-id (+ valuation-count u1))
      (max-change (* current-value u2))
      (min-change (/ current-value u2))
    )
    (asserts! (is-eq tx-sender (get owner property)) err-not-authorized)
    (asserts! (> new-value u0) err-invalid-valuation)
    (asserts! (<= new-value max-change) err-valuation-change-too-large)
    (asserts! (>= new-value min-change) err-valuation-change-too-large)
    (map-set properties
      { property-id: property-id }
      (merge property { 
        total-value: new-value,
        price-per-token: new-price-per-token
      })
    )
    (map-set property-valuations
      { property-id: property-id, valuation-id: next-valuation-id }
      {
        new-value: new-value,
        old-value: current-value,
        valuation-date: burn-block-height,
        updated-by: tx-sender
      }
    )
    (map-set property-valuation-count
      { property-id: property-id }
      { count: next-valuation-id }
    )
    (ok new-value)
  )
)

(define-public (deposit-rental-income (property-id uint) (rental-amount uint) (period-start uint) (period-end uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
      (rental-count (default-to u0 (get count (map-get? rental-period-count { property-id: property-id }))))
      (next-period-id (+ rental-count u1))
      (total-tokens-issued (- (get total-tokens property) (get available-tokens property)))
    )
    (asserts! (is-eq tx-sender (get owner property)) err-not-authorized)
    (asserts! (> rental-amount u0) err-invalid-amount)
    (asserts! (> total-tokens-issued u0) err-insufficient-tokens)
    (asserts! (< period-start period-end) err-invalid-amount)
    (try! (stx-transfer? rental-amount tx-sender (as-contract tx-sender)))
    (map-set rental-periods
      { property-id: property-id, period-id: next-period-id }
      {
        rental-amount: rental-amount,
        period-start: period-start,
        period-end: period-end,
        total-tokens-snapshot: total-tokens-issued,
        distributed: false
      }
    )
    (map-set rental-period-count
      { property-id: property-id }
      { count: next-period-id }
    )
    (ok next-period-id)
  )
)

(define-public (claim-rental-income (property-id uint) (period-id uint))
  (let
    (
      (rental-period (unwrap! (map-get? rental-periods { property-id: property-id, period-id: period-id }) err-rental-period-not-found))
      (user-tokens (get-user-tokens property-id tx-sender))
      (total-tokens-snapshot (get total-tokens-snapshot rental-period))
      (rental-amount (get rental-amount rental-period))
      (claim-record (map-get? rental-claims { property-id: property-id, period-id: period-id, holder: tx-sender }))
      (user-share (/ (* user-tokens rental-amount) total-tokens-snapshot))
    )
    (asserts! (> user-tokens u0) err-insufficient-tokens)
    (asserts! (is-none claim-record) err-already-claimed)
    (asserts! (> rental-amount u0) err-no-rental-income)
    (try! (as-contract (stx-transfer? user-share tx-sender tx-sender)))
    (map-set rental-claims
      { property-id: property-id, period-id: period-id, holder: tx-sender }
      {
        amount-claimed: user-share,
        claimed: true
      }
    )
    (ok user-share)
  )
)

(define-read-only (get-unclaimed-rental-for-period (property-id uint) (period-id uint) (holder principal))
  (let
    (
      (rental-period (map-get? rental-periods { property-id: property-id, period-id: period-id }))
      (claim-record (map-get? rental-claims { property-id: property-id, period-id: period-id, holder: holder }))
      (user-tokens (get-user-tokens property-id holder))
    )
    (match rental-period
      period
        (if (is-none claim-record)
          (let
            (
              (rental-amount (get rental-amount period))
              (total-tokens-snapshot (get total-tokens-snapshot period))
            )
            (if (> total-tokens-snapshot u0)
              (some (/ (* user-tokens rental-amount) total-tokens-snapshot))
              none
            )
          )
          none
        )
      none
    )
  )
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-listed-property (property-id uint))
  (let
    (
      (property (map-get? properties { property-id: property-id }))
    )
    (match property
      prop (if (get is-listed prop) (some prop) none)
      none
    )
  )
)

(define-read-only (get-user-tokens (property-id uint) (user principal))
  (default-to u0 (get amount (map-get? property-tokens { property-id: property-id, holder: user })))
)

(define-read-only (get-user-properties (user principal) (property-id uint))
  (map-get? user-properties { user: user, property-id: property-id })
)

(define-read-only (get-property-ownership-percentage (property-id uint) (user principal))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
      (user-tokens (get-user-tokens property-id user))
      (total-tokens (get total-tokens property))
    )
    (if (> total-tokens u0)
      (ok (/ (* user-tokens u10000) total-tokens))
      err-invalid-amount
    )
  )
)

(define-read-only (get-next-property-id)
  (var-get next-property-id)
)

(define-read-only (calculate-token-value (property-id uint) (token-amount uint))
  (let
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
    (ok (* (get price-per-token property) token-amount))
  )
)

(define-read-only (get-total-properties)
  (- (var-get next-property-id) u1)
)

(define-read-only (get-property-listing-status (property-id uint))
  (let
    (
      (property (map-get? properties { property-id: property-id }))
    )
    (match property
      prop (some (get is-listed prop))
      none
    )
  )
)

(define-read-only (get-property-valuation-history (property-id uint) (valuation-id uint))
  (map-get? property-valuations { property-id: property-id, valuation-id: valuation-id })
)

(define-read-only (get-property-valuation-count (property-id uint))
  (default-to u0 (get count (map-get? property-valuation-count { property-id: property-id })))
)

(define-read-only (get-latest-property-valuation (property-id uint))
  (let
    (
      (valuation-count (get-property-valuation-count property-id))
    )
    (if (> valuation-count u0)
      (map-get? property-valuations { property-id: property-id, valuation-id: valuation-count })
      none
    )
  )
)

(define-read-only (calculate-valuation-change-percentage (property-id uint))
  (let
    (
      (latest-valuation (get-latest-property-valuation property-id))
    )
    (match latest-valuation
      valuation 
        (let
          (
            (new-val (get new-value valuation))
            (old-val (get old-value valuation))
          )
          (if (> old-val u0)
            (some (/ (* (- new-val old-val) u10000) old-val))
            none
          )
        )
      none
    )
  )
)

(define-read-only (get-rental-period (property-id uint) (period-id uint))
  (map-get? rental-periods { property-id: property-id, period-id: period-id })
)

(define-read-only (get-rental-period-count (property-id uint))
  (default-to u0 (get count (map-get? rental-period-count { property-id: property-id })))
)

(define-read-only (get-rental-claim (property-id uint) (period-id uint) (holder principal))
  (map-get? rental-claims { property-id: property-id, period-id: period-id, holder: holder })
)

(define-read-only (get-total-rental-for-property (property-id uint))
  (let
    (
      (rental-count (get-rental-period-count property-id))
      (period-1 (map-get? rental-periods { property-id: property-id, period-id: u1 }))
      (period-2 (map-get? rental-periods { property-id: property-id, period-id: u2 }))
      (period-3 (map-get? rental-periods { property-id: property-id, period-id: u3 }))
      (period-4 (map-get? rental-periods { property-id: property-id, period-id: u4 }))
      (period-5 (map-get? rental-periods { property-id: property-id, period-id: u5 }))
    )
    (+ 
      (match period-1 p1 (get rental-amount p1) u0)
      (match period-2 p2 (get rental-amount p2) u0)
      (match period-3 p3 (get rental-amount p3) u0)
      (match period-4 p4 (get rental-amount p4) u0)
      (match period-5 p5 (get rental-amount p5) u0)
    )
  )
)

(define-public (create-sell-offer (property-id uint) (token-amount uint) (price-per-token uint) (expiry-blocks uint))
  (let
    (
      (user-tokens (get-user-tokens property-id tx-sender))
      (offer-count (default-to u0 (get count (map-get? user-sell-offer-count { property-id: property-id, seller: tx-sender }))))
      (next-offer-id (+ offer-count u1))
      (expiry-block (+ burn-block-height expiry-blocks))
    )
    (asserts! (>= user-tokens token-amount) err-insufficient-tokens)
    (asserts! (> token-amount u0) err-invalid-amount)
    (asserts! (> price-per-token u0) err-invalid-price)
    (asserts! (> expiry-blocks u0) err-invalid-amount)
    (map-set sell-offers
      { property-id: property-id, seller: tx-sender, offer-id: next-offer-id }
      {
        token-amount: token-amount,
        price-per-token: price-per-token,
        expiry-block: expiry-block,
        is-active: true
      }
    )
    (map-set user-sell-offer-count
      { property-id: property-id, seller: tx-sender }
      { count: next-offer-id }
    )
    (ok next-offer-id)
  )
)

(define-public (create-buy-offer (property-id uint) (token-amount uint) (price-per-token uint) (expiry-blocks uint))
  (let
    (
      (total-cost (* token-amount price-per-token))
      (offer-count (default-to u0 (get count (map-get? user-buy-offer-count { property-id: property-id, buyer: tx-sender }))))
      (next-offer-id (+ offer-count u1))
      (expiry-block (+ burn-block-height expiry-blocks))
    )
    (asserts! (> token-amount u0) err-invalid-amount)
    (asserts! (> price-per-token u0) err-invalid-price)
    (asserts! (> expiry-blocks u0) err-invalid-amount)
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    (map-set buy-offers
      { property-id: property-id, buyer: tx-sender, offer-id: next-offer-id }
      {
        token-amount: token-amount,
        price-per-token: price-per-token,
        expiry-block: expiry-block,
        is-active: true
      }
    )
    (map-set user-buy-offer-count
      { property-id: property-id, buyer: tx-sender }
      { count: next-offer-id }
    )
    (ok next-offer-id)
  )
)

(define-public (accept-sell-offer (property-id uint) (seller principal) (offer-id uint) (token-amount uint))
  (let
    (
      (sell-offer (unwrap! (map-get? sell-offers { property-id: property-id, seller: seller, offer-id: offer-id }) err-offer-not-found))
      (price-per-token (get price-per-token sell-offer))
      (total-cost (* token-amount price-per-token))
      (seller-tokens (get-user-tokens property-id seller))
      (buyer-tokens (get-user-tokens property-id tx-sender))
      (seller-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: seller, property-id: property-id }))))
      (buyer-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: tx-sender, property-id: property-id }))))
    )
    (asserts! (get is-active sell-offer) err-offer-not-found)
    (asserts! (<= burn-block-height (get expiry-block sell-offer)) err-offer-expired)
    (asserts! (<= token-amount (get token-amount sell-offer)) err-insufficient-tokens)
    (asserts! (>= seller-tokens token-amount) err-insufficient-tokens)
    (asserts! (> token-amount u0) err-invalid-amount)
    (try! (stx-transfer? total-cost tx-sender seller))
    (map-set property-tokens
      { property-id: property-id, holder: seller }
      { amount: (- seller-tokens token-amount) }
    )
    (map-set property-tokens
      { property-id: property-id, holder: tx-sender }
      { amount: (+ buyer-tokens token-amount) }
    )
    (map-set user-properties
      { user: seller, property-id: property-id }
      { tokens-owned: (- seller-user-tokens token-amount) }
    )
    (map-set user-properties
      { user: tx-sender, property-id: property-id }
      { tokens-owned: (+ buyer-user-tokens token-amount) }
    )
    (if (is-eq token-amount (get token-amount sell-offer))
      (map-set sell-offers
        { property-id: property-id, seller: seller, offer-id: offer-id }
        (merge sell-offer { is-active: false })
      )
      (map-set sell-offers
        { property-id: property-id, seller: seller, offer-id: offer-id }
        (merge sell-offer { token-amount: (- (get token-amount sell-offer) token-amount) })
      )
    )
    (ok token-amount)
  )
)

(define-public (accept-buy-offer (property-id uint) (buyer principal) (offer-id uint) (token-amount uint))
  (let
    (
      (buy-offer (unwrap! (map-get? buy-offers { property-id: property-id, buyer: buyer, offer-id: offer-id }) err-offer-not-found))
      (price-per-token (get price-per-token buy-offer))
      (total-payment (* token-amount price-per-token))
      (seller-tokens (get-user-tokens property-id tx-sender))
      (buyer-tokens (get-user-tokens property-id buyer))
      (seller-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: tx-sender, property-id: property-id }))))
      (buyer-user-tokens (default-to u0 (get tokens-owned (map-get? user-properties { user: buyer, property-id: property-id }))))
    )
    (asserts! (get is-active buy-offer) err-offer-not-found)
    (asserts! (<= burn-block-height (get expiry-block buy-offer)) err-offer-expired)
    (asserts! (<= token-amount (get token-amount buy-offer)) err-insufficient-tokens)
    (asserts! (>= seller-tokens token-amount) err-insufficient-tokens)
    (asserts! (> token-amount u0) err-invalid-amount)
    (try! (as-contract (stx-transfer? total-payment tx-sender tx-sender)))
    (map-set property-tokens
      { property-id: property-id, holder: tx-sender }
      { amount: (- seller-tokens token-amount) }
    )
    (map-set property-tokens
      { property-id: property-id, holder: buyer }
      { amount: (+ buyer-tokens token-amount) }
    )
    (map-set user-properties
      { user: tx-sender, property-id: property-id }
      { tokens-owned: (- seller-user-tokens token-amount) }
    )
    (map-set user-properties
      { user: buyer, property-id: property-id }
      { tokens-owned: (+ buyer-user-tokens token-amount) }
    )
    (if (is-eq token-amount (get token-amount buy-offer))
      (map-set buy-offers
        { property-id: property-id, buyer: buyer, offer-id: offer-id }
        (merge buy-offer { is-active: false })
      )
      (map-set buy-offers
        { property-id: property-id, buyer: buyer, offer-id: offer-id }
        (merge buy-offer { token-amount: (- (get token-amount buy-offer) token-amount) })
      )
    )
    (ok token-amount)
  )
)

(define-public (cancel-sell-offer (property-id uint) (offer-id uint))
  (let
    (
      (sell-offer (unwrap! (map-get? sell-offers { property-id: property-id, seller: tx-sender, offer-id: offer-id }) err-offer-not-found))
    )
    (asserts! (get is-active sell-offer) err-offer-not-found)
    (map-set sell-offers
      { property-id: property-id, seller: tx-sender, offer-id: offer-id }
      (merge sell-offer { is-active: false })
    )
    (ok true)
  )
)

(define-public (cancel-buy-offer (property-id uint) (offer-id uint))
  (let
    (
      (buy-offer (unwrap! (map-get? buy-offers { property-id: property-id, buyer: tx-sender, offer-id: offer-id }) err-offer-not-found))
      (refund-amount (* (get token-amount buy-offer) (get price-per-token buy-offer)))
    )
    (asserts! (get is-active buy-offer) err-offer-not-found)
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    (map-set buy-offers
      { property-id: property-id, buyer: tx-sender, offer-id: offer-id }
      (merge buy-offer { is-active: false })
    )
    (ok true)
  )
)

(define-read-only (get-sell-offer (property-id uint) (seller principal) (offer-id uint))
  (map-get? sell-offers { property-id: property-id, seller: seller, offer-id: offer-id })
)

(define-read-only (get-buy-offer (property-id uint) (buyer principal) (offer-id uint))
  (map-get? buy-offers { property-id: property-id, buyer: buyer, offer-id: offer-id })
)

(define-read-only (get-user-sell-offer-count (property-id uint) (seller principal))
  (default-to u0 (get count (map-get? user-sell-offer-count { property-id: property-id, seller: seller })))
)

(define-read-only (get-user-buy-offer-count (property-id uint) (buyer principal))
  (default-to u0 (get count (map-get? user-buy-offer-count { property-id: property-id, buyer: buyer })))
)

(define-read-only (is-sell-offer-active (property-id uint) (seller principal) (offer-id uint))
  (let
    (
      (offer (map-get? sell-offers { property-id: property-id, seller: seller, offer-id: offer-id }))
    )
    (match offer
      o (and (get is-active o) (<= burn-block-height (get expiry-block o)))
      false
    )
  )
)

(define-read-only (is-buy-offer-active (property-id uint) (buyer principal) (offer-id uint))
  (let
    (
      (offer (map-get? buy-offers { property-id: property-id, buyer: buyer, offer-id: offer-id }))
    )
    (match offer
      o (and (get is-active o) (<= burn-block-height (get expiry-block o)))
      false
    )
  )
)

(define-read-only (get-sell-offer-value (property-id uint) (seller principal) (offer-id uint))
  (let
    (
      (offer (map-get? sell-offers { property-id: property-id, seller: seller, offer-id: offer-id }))
    )
    (match offer
      o (some (* (get token-amount o) (get price-per-token o)))
      none
    )
  )
)

(define-read-only (get-buy-offer-value (property-id uint) (buyer principal) (offer-id uint))
  (let
    (
      (offer (map-get? buy-offers { property-id: property-id, buyer: buyer, offer-id: offer-id }))
    )
    (match offer
      o (some (* (get token-amount o) (get price-per-token o)))
      none
    )
  )
)