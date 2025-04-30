;; Clarity Harmony Commons
;; This contract provides functionality for managing association members,
;; their profiles, and personalization settings.
;; It allows for secure management of member data and access control.

;; =============================================
;; DATA STORAGE STRUCTURES
;; =============================================

;; Central repository for participant information
(define-map participant-registry
  { participant-id: uint }
  {
    display-name: (string-ascii 50),
    account-principal: principal,
    enrollment-block: uint,
    personal-description: (string-ascii 160),
    interest-tags: (list 5 (string-ascii 30))
  }
)

;; Configuration for who can view participant details
(define-map information-access-controls
  { participant-id: uint, accessor-principal: principal }
  { access-enabled: bool }
)

;; Historical activity records for participants
(define-map participant-engagement-history
  { participant-id: uint }
  {
    last-visit: uint,
    visit-count: uint,
    recent-activity: (string-ascii 50)
  }
)

;; =============================================
;; UTILITY FUNCTIONS
;; =============================================

;; Determine if a participant record exists
(define-private (participant-record-exists? (participant-id uint))
  (is-some (map-get? participant-registry { participant-id: participant-id }))
)

;; Validate ownership of participant record
(define-private (is-account-owner? (participant-id uint) (account-address principal))
  (match (map-get? participant-registry { participant-id: participant-id })
    profile-data (is-eq (get account-principal profile-data) account-address)
    false
  )
)

;; Validate a single interest tag
(define-private (is-tag-valid? (tag (string-ascii 30)))
  (and
    (> (len tag) u0)
    (< (len tag) u31)
  )
)

;; Ensure all interest tags meet requirements
(define-private (are-tags-acceptable? (tags (list 5 (string-ascii 30))))
  (and
    (> (len tags) u0)
    (<= (len tags) u5)
    (is-eq (len (filter is-tag-valid? tags)) (len tags))
  )
)

;; =============================================
;; CONSTANTS AND ERROR CODES
;; =============================================

;; Platform error codes
(define-constant ERROR-ACCESS-DENIED (err u500))
(define-constant ERROR-RECORD-MISSING (err u501))
(define-constant ERROR-DUPLICATE-ENTRY (err u502))
(define-constant ERROR-VALIDATION-FAILED (err u503))
(define-constant ERROR-OPERATION-RESTRICTED (err u504))

;; Administrative settings
(define-constant PLATFORM-ADMINISTRATOR tx-sender)

;; =============================================
;; STATE VARIABLES
;; =============================================

;; Current total participants in the system
(define-data-var total-participants uint u0)


;; =============================================
;; CORE PARTICIPANT MANAGEMENT
;; =============================================

;; Enroll a new participant in the system
(define-public (enroll-participant 
    (display-name (string-ascii 50)) 
    (personal-description (string-ascii 160)) 
    (interest-tags (list 5 (string-ascii 30))))
  (let
    (
      (new-id (+ (var-get total-participants) u1))
    )
    ;; Input validation checks
    (asserts! (and (> (len display-name) u0) (< (len display-name) u51)) ERROR-VALIDATION-FAILED)
    (asserts! (and (> (len personal-description) u0) (< (len personal-description) u161)) ERROR-VALIDATION-FAILED)
    (asserts! (are-tags-acceptable? interest-tags) ERROR-VALIDATION-FAILED)

    ;; Create new participant profile
    (map-insert participant-registry
      { participant-id: new-id }
      {
        display-name: display-name,
        account-principal: tx-sender,
        enrollment-block: block-height,
        personal-description: personal-description,
        interest-tags: interest-tags
      }
    )

    ;; Initialize access permissions for owner
    (map-insert information-access-controls
      { participant-id: new-id, accessor-principal: tx-sender }
      { access-enabled: true }
    )

    ;; Update system participant count
    (var-set total-participants new-id)
    (ok new-id)
  )
)

;; =============================================
;; PROFILE MANAGEMENT FUNCTIONS
;; =============================================

;; Modify participant interest tags
(define-public (modify-interest-tags (participant-id uint) (new-tags (list 5 (string-ascii 30))))
  (let
    (
      (profile-data (unwrap! (map-get? participant-registry { participant-id: participant-id }) ERROR-RECORD-MISSING))
    )
    ;; Security and validation checks
    (asserts! (participant-record-exists? participant-id) ERROR-RECORD-MISSING)
    (asserts! (is-eq (get account-principal profile-data) tx-sender) ERROR-OPERATION-RESTRICTED)
    (asserts! (are-tags-acceptable? new-tags) ERROR-VALIDATION-FAILED)

    ;; Update the interest tags
    (map-set participant-registry
      { participant-id: participant-id }
      (merge profile-data { interest-tags: new-tags })
    )
    (ok true)
  )
)

;; Add new participant with complete profile details
(define-public (register-complete-profile 
    (display-name (string-ascii 50)) 
    (personal-description (string-ascii 160)) 
    (interest-tags (list 5 (string-ascii 30))))
  (let
    (
      (new-id (+ (var-get total-participants) u1))
    )
    ;; Validate all inputs
    (asserts! (and (> (len display-name) u0) (< (len display-name) u51)) ERROR-VALIDATION-FAILED)
    (asserts! (and (> (len personal-description) u0) (< (len personal-description) u161)) ERROR-VALIDATION-FAILED)
    (asserts! (are-tags-acceptable? interest-tags) ERROR-VALIDATION-FAILED)

    ;; Store participant data
    (map-insert participant-registry
      { participant-id: new-id }
      {
        display-name: display-name,
        account-principal: tx-sender,
        enrollment-block: block-height,
        personal-description: personal-description,
        interest-tags: interest-tags
      }
    )

    ;; Configure initial access permissions
    (map-insert information-access-controls
      { participant-id: new-id, accessor-principal: tx-sender }
      { access-enabled: true }
    )

    ;; Update participant counter
    (var-set total-participants new-id)
    (ok new-id)
  )
)

;; Update participant display name
(define-public (change-display-name (participant-id uint) (new-display-name (string-ascii 50)))
  (let
    (
      (profile-data (unwrap! (map-get? participant-registry { participant-id: participant-id }) ERROR-RECORD-MISSING))
    )
    ;; Validation and security checks
    (asserts! (participant-record-exists? participant-id) ERROR-RECORD-MISSING)
    (asserts! (is-eq (get account-principal profile-data) tx-sender) ERROR-OPERATION-RESTRICTED)

    ;; Update display name
    (map-set participant-registry
      { participant-id: participant-id }
      (merge profile-data { display-name: new-display-name })
    )
    (ok true)
  )
)

;; =============================================
;; ENHANCED PROFILE MANAGEMENT
;; =============================================

;; Optimized interest tags update function
(define-public (quick-update-interests (participant-id uint) (new-tags (list 5 (string-ascii 30))))
  (begin
    (asserts! (participant-record-exists? participant-id) ERROR-RECORD-MISSING)
    (asserts! (are-tags-acceptable? new-tags) ERROR-VALIDATION-FAILED)
    (map-set participant-registry
      { participant-id: participant-id }
      (merge (unwrap! (map-get? participant-registry { participant-id: participant-id }) ERROR-RECORD-MISSING) 
             { interest-tags: new-tags })
    )
    (ok "Interest tags successfully updated")
  )
)

;; Control access to participant profiles
(define-public (set-profile-access-restrictions (participant-id uint) (accessor-address principal))
  (let
    (
      (profile-data (unwrap! (map-get? participant-registry { participant-id: participant-id }) ERROR-RECORD-MISSING))
    )
    ;; Verify authorization
    (asserts! (is-eq (get account-principal profile-data) accessor-address) ERROR-OPERATION-RESTRICTED)
    (ok true)
  )
)

;; Comprehensive profile update with validation
(define-public (comprehensive-profile-update 
    (participant-id uint) 
    (new-display-name (string-ascii 50)) 
    (new-description (string-ascii 160)) 
    (new-interest-tags (list 5 (string-ascii 30))))
  (let
    (
      (profile-data (unwrap! (map-get? participant-registry { participant-id: participant-id }) ERROR-RECORD-MISSING))
    )
    ;; Extensive validation
    (asserts! (participant-record-exists? participant-id) ERROR-RECORD-MISSING)
    (asserts! (is-eq (get account-principal profile-data) tx-sender) ERROR-OPERATION-RESTRICTED)
    (asserts! (> (len new-display-name) u0) ERROR-VALIDATION-FAILED)
    (asserts! (< (len new-display-name) u51) ERROR-VALIDATION-FAILED)
    (asserts! (are-tags-acceptable? new-interest-tags) ERROR-VALIDATION-FAILED)

    ;; Update all profile fields
    (map-set participant-registry
      { participant-id: participant-id }
      (merge profile-data { 
        display-name: new-display-name, 
        personal-description: new-description, 
        interest-tags: new-interest-tags 
      })
    )
    (ok true)
  )
)

;; =============================================
;; ACCOUNT VERIFICATION & ACTIVITY TRACKING
;; =============================================

;; Confirm ownership of participant profile
(define-public (authenticate-profile-ownership (participant-id uint) (claiming-address principal))
  (let
    (
      (profile-data (unwrap! (map-get? participant-registry { participant-id: participant-id }) ERROR-RECORD-MISSING))
    )
    (ok (is-eq claiming-address (get account-principal profile-data)))
  )
)

;; Record participant platform activity
(define-public (track-participant-session (participant-id uint))
  (let
    (
      (current-history (default-to 
        { last-visit: u0, visit-count: u0, recent-activity: "None" }
        (map-get? participant-engagement-history { participant-id: participant-id })))
    )
    (asserts! (participant-record-exists? participant-id) ERROR-RECORD-MISSING)
    (map-set participant-engagement-history
      { participant-id: participant-id }
      {
        last-visit: block-height,
        visit-count: (+ (get visit-count current-history) u1),
        recent-activity: "platform-login"
      }
    )
    (ok true)
  )
)

