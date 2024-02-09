(defun drm:elfeed-inc-score (entry)
  (elfeed-score-serde-add-rule (elfeed-score-title-rule--create
				:text (string-trim-left (elfeed-entry-title entry) "re: +")
				:value 1
				:type 's
				:comment "ADAPTIVE")
			       )
  (elfeed-score-load-score-file elfeed-score-serde-score-file)
  )
(advice-add 'elfeed-search-show-entry :after 'drm:elfeed-inc-score)

(defun drm:elfeed-dec-score ()
  (let ((entry (car (elfeed-search-selected))))
    (elfeed-score-serde-add-rule (elfeed-score-title-rule--create
				  :text (string-trim-left (elfeed-entry-title entry) "re: +" )
				  :value -1
				  :type 's
				  :comment "ADAPTIVE")
				 )
    (elfeed-score-load-score-file elfeed-score-serde-score-file)
    )
  )
(advice-add 'elfeed-search-untag-all-unread :after 'drm:elfeed-dec-score)

