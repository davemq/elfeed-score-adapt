;; Add adaptive scoring to elfeed-score.

(require 'elfeed)
(require 'elfeed-score)

(provide 'elfeed-score-adaptive)

;; Eventually we want a way to configure the scores that get added for
;; different elfeed actions
(defvar elfeed-score-adaptive-alist
  '((read
     (title -10))
    (show
     (authors 5)
     (feed 2)
     (title 30))
    )
   "List of score actions to take for different elfeed-search entry actions"
  )

(defun elfeed-score-adaptive-add-rules (action entry)
  "Add rules for entry based on items for action in `elfeed-score-adaptive-alist'."
  (dolist (elt (cdr (assoc action elfeed-score-adaptive-alist)))
    (let ((type (car elt))
	  (value (car (cdr elt))))
      (cond ((eq type 'authors)
	     (let ((a (elfeed-score-scoring--concatenate-authors
                    (elfeed-meta entry :authors))))
	       (elfeed-score-adaptive-add-authors-rule a value)))
	    ((eq type 'title)
 	     (let ((tt (string-trim-left (elfeed-entry-title entry) "re: +")))
	       (elfeed-score-adaptive-add-title-rule tt value)))
	    ((eq type 'feed)
	     (let ((feed (elfeed-feed-url (elfeed-entry-feed entry))))
	       (elfeed-score-adaptive-add-feed-rule feed value)))
	     )
	    )
      )
    )

(defun elfeed-score-adaptive-add-read-rules (entry)
  "Add `'read` rules"
  (elfeed-score-adaptive-add-rules 'read entry)
  )

(defun elfeed-score-adaptive-add-title-rule (title value)
  "Add a title rule"
  (ignore-errors
    (elfeed-score-serde-add-rule (elfeed-score-title-rule--create
				  :text title
				  :value value
				  :type 's
				  :comment "ADAPTIVE")
				 )
    )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )
  
(defun elfeed-score-adaptive-add-authors-rule (authors value)
  "Add an authors rule"
  (ignore-errors
    (elfeed-score-serde-add-rule (elfeed-score-authors-rule--create
				  :text authors
				  :value value
				  :type 's
				  :comment "ADAPTIVE")
				 )
    )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )

(defun elfeed-score-adaptive-add-feed-rule (feed value)
  "Add an feed rule"
  (ignore-errors
    (elfeed-score-serde-add-rule (elfeed-score-feed-rule--create
				  :text feed
				  :attr 'u
				  :value value
				  :type 's
				  :comment "ADAPTIVE")
				 )
    )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )

(defun elfeed-score-adaptive-show (entry)
  (elfeed-score-adaptive-add-rules 'show entry)
  )

(defun elfeed-score-adaptive-read ()
  (let ((entries (elfeed-search-selected)))
    (mapc #'elfeed-score-adaptive-add-read-rules entries)
    )
  )

;; It would be nice if there were hooks for the various actions in the
;; *elfeed-search* buffer, e.g. RET shows an entry, "r" untags unread entries,
;; i.e. marks them as read. I think these are the ones I care about for now.
;; The functions they call don't have hooks, so we'll have to advise them.

;; For "show" (RET), advise elfeed-search-
(advice-add (lookup-key elfeed-search-mode-map (kbd "RET"))
            :after
            #'elfeed-score-adaptive-show
            )

;; For "untag" ("r"), advise elfeed-search-untag-all-unread
(advice-add (lookup-key elfeed-search-mode-map "r")
            :before
            #'elfeed-score-adaptive-read
            )
