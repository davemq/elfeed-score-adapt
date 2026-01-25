;;; ...  -*- lexical-binding: t -*-

;; Add adaptive scoring to elfeed-score.

(require 'elfeed)
(require 'elfeed-score)
(require 'time-date)

(provide 'elfeed-score-adapt)

;; Eventually we want a way to configure the scores that get added for
;; different elfeed actions
(defvar elfeed-score-adapt-alist
  '((read
     (title -10))
    (show
     (authors 5)
     (feed 2)
     (title 30))
    )
   "List of score actions to take for different elfeed-search entry actions"
  )

;; Expiry days
(defvar elfeed-score-adapt-expiry-days
  7
  "Number of days to maintain adaptive score information"
)


(defun elfeed-score-adapt-add-rules (action entry)
  "Add rules for entry based on items for action in `elfeed-score-adapt-alist'."
  (dolist (elt (cdr (assoc action elfeed-score-adapt-alist)))
    (let ((type (car elt))
	  (value (car (cdr elt))))
      (cond ((eq type 'authors)
	     (let ((a (elfeed-score-scoring--concatenate-authors
                    (elfeed-meta entry :authors))))
	       (elfeed-score-adapt-add-authors-rule a value)))
	    ((eq type 'title)
 	     (let ((tt (string-trim-left (elfeed-entry-title entry) "re: +")))
	       (elfeed-score-adapt-add-title-rule tt value)))
	    ((eq type 'feed)
	     (let ((feed (elfeed-feed-url (elfeed-entry-feed entry))))
	       (elfeed-score-adapt-add-feed-rule feed value)))
	     )
	    )
      )
    )

(defun elfeed-score-adapt-add-read-rules (entry)
  "Add `'read` rules"
  (elfeed-score-adapt-add-rules 'read entry)
  )

(defun elfeed-score-adapt-get-comment ()
  (list 'adapt (time-to-days nil)))

(defun elfeed-score-adapt-add-title-rule (title value)
  "Add a title rule"
  (ignore-errors
    (elfeed-score-serde-add-rule (elfeed-score-title-rule--create
				  :text title
				  :value value
				  :type 's
				  :comment (elfeed-score-adapt-get-comment))
				 )
    )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )
  
(defun elfeed-score-adapt-add-authors-rule (authors value)
  "Add an authors rule"
  (ignore-errors
    (elfeed-score-serde-add-rule (elfeed-score-authors-rule--create
				  :text authors
				  :value value
				  :type 's
				  :comment (elfeed-score-adapt-get-comment))
				 )
    )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )

(defun elfeed-score-adapt-add-feed-rule (feed value)
  "Add an feed rule"
  (ignore-errors
    (elfeed-score-serde-add-rule (elfeed-score-feed-rule--create
				  :text feed
				  :attr 'u
				  :value value
				  :type 's
				  :comment (elfeed-score-adapt-get-comment))
				 )
    )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )

(defun elfeed-score-adapt-show (entry)
  (elfeed-score-adapt-add-rules 'show entry)
  )

(defun elfeed-score-adapt-read ()
  (let ((entries (elfeed-search-selected)))
    (mapc #'elfeed-score-adapt-add-read-rules entries)
    )
  )

(defun elfeed-score-adapt-enable ()
  "Enable adaptive scoring for elfeed."

  (interactive)
  ;; It would be nice if there were hooks for the various actions in the
  ;; *elfeed-search* buffer, e.g. RET shows an entry, "r" untags unread entries,
  ;; i.e. marks them as read. I think these are the ones I care about for now.
  ;; The functions they call don't have hooks, so we'll have to advise them.

  ;; For "show" (RET), advise elfeed-search-
  (advice-add (lookup-key elfeed-search-mode-map (kbd "RET"))
              :after
              #'elfeed-score-adapt-show
              )

  ;; For "untag" ("r"), advise elfeed-search-untag-all-unread
  (advice-add (lookup-key elfeed-search-mode-map "r")
              :before
              #'elfeed-score-adapt-read
              )
  )

(defun elfeed-score-adapt-disable ()
  "Disable adaptive scoring for elfeed."

  (interactive)

  (advice-remove (lookup-key elfeed-search-mode-map (kbd "RET"))
		 #'elfeed-score-adapt-show)

  (advice-remove (lookup-key elfeed-search-mode-map "r")
		 #'elfeed-score-adapt-read)
  )

(defun elfeed-score-adapt-get-days (e)
  "Get adaptive info from entry"
  (let*
      ((p (elfeed-score-serde-struct-to-plist e))
       (c (plist-get p :comment))
       (days (plist-get c 'adapt)))
    days
    )
  )

(defun elfeed-score-adapt-expirable (e)
  "Determine if e is expirable."
  (let ((days (elfeed-score-adapt-get-days e))
	(today (time-to-days nil)))
    (if days
	(<= days (- today 7))
      nil
      )
    )
  )

(defun elfeed-score-adapt-expire-list (l)
  "Expire entries by filtering out old, i.e. expirable, items."
  (seq-remove #'elfeed-score-adapt-expirable l)
  )

(defun elfeed-score-adapt-expire ()
  "Expire entries with adaptive score information older than
`elfeed-score-adapt-expiry-days' days."
  (interactive)
  (let ((lists '(elfeed-score-serde-title-rules
		 elfeed-score-serde-feed-rules
		 elfeed-score-serde-authors-rules
		 elfeed-score-serde-content-rules
		 elfeed-score-serde-title-or-content-rules
		 elfeed-score-serde-tag-rules
		 elfeed-score-serde-link-rules
		 elfeed-score-serde-udf-rules
		 elfeed-score-serde-adjust-tags-rules)))
    (let ((l (car lists)))
      (while l
	(eval `(setq ,l (elfeed-score-adapt-expire-list ,l)))
	(setq lists (cdr lists))
	(setq l (car lists))
	)
      )
    )
  (if elfeed-score-serde-score-file
      (elfeed-score-serde-write-score-file elfeed-score-serde-score-file)
    )
  )
