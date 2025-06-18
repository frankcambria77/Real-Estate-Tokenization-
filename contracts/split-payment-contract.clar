(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-not-authorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-property-not-active (err u106))
(define-constant err-insufficient-tokens (err u107))

(define-map properties
  { property-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    total-value: uint,
    total-tokens: uint,
    available-tokens: uint,
    price-per-token: uint,
    is-active: bool
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
        is-active: true
      }
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

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
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