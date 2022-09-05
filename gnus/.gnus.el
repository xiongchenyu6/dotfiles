(setq user-full-name "XiongChenYu"
      user-mail-address "xiongchenyu6@gmail.com")

(setq message-send-mail-function 'smtpmail-send-it
  smtpmail-stream-type 'starttls
  smtpmail-default-smtp-server "smtp.gmail.com"
  smtpmail-smtp-server "smtp.gmail.com"
  smtpmail-smtp-service 587)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-select-method '(nntp "news.gwene.org"))

(add-to-list 'gnus-secondary-select-methods
      '(nnimap "gmail"
        (nnimap-address "imap.gmail.com")
        (nnimap-server-port 993)
        (nnimap-stream ssl)))


(setq gnus-extra-headers
      '(To Newsgroups X-GM-LABELS))

;; 使用准确率较高的地址提取函数
(setq gnus-extract-address-components
      'mail-extract-address-components)

;; 默认禁用 nnfolder
(setq gnus-message-archive-group nil)

;; 双窗口布局(水平)
(gnus-add-configuration
 '(article
   (vertical 1.0
             (summary 0.25 point)
             (article 1.0))))

;; 设置图片显示方式

(setq mm-inline-large-images t)

;; 设置summary缓冲区的显示格式
(setq gnus-extra-headers '(To From))
(setq nnmail-extra-headers gnus-extra-headers)
(setq gnus-summary-gather-subject-limit 'fuzzy)
(setq gnus-summary-make-false-root 'adopt)
(setq gnus-summary-line-format "%U%R%z %&user-date;  %-16,16a %5k%I%B%s\n")

;; 设置 threads 的样式
(setq gnus-thread-indent-level 0)
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-single-indent "  ")
(setq gnus-sum-thread-tree-root "  ")
(setq gnus-sum-thread-tree-false-root "  ")
(setq gnus-sum-thread-tree-vertical "   |")
(setq gnus-sum-thread-tree-leaf-with-other "   |-> ")
(setq gnus-sum-thread-tree-single-leaf "    `-> ")

;; 设置 `gnus-summary-line-format' 中的 %&user-date;
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . " %H:%M")
        ((gnus-seconds-month) . "  %d day")
        ((gnus-seconds-year)  . " %m-%d")
        (t . "%Y year")))

;; 将邮件的发出时间转换为本地时间
(add-hook 'gnus-article-prepare-hook #'gnus-article-date-local)

;; 跟踪组的时间轴
(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)


;; visual
(setq gnus-treat-emphasize t
      gnus-treat-buttonize t
      gnus-treat-buttonize-head 'head
      gnus-treat-unsplit-urls 'last
      gnus-treat-leading-whitespace 'head
      gnus-treat-highlight-citation t
      gnus-treat-highlight-signature t
      gnus-treat-date-lapsed 'head
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-strip-cr t
      gnus-treat-overstrike nil
      gnus-treat-display-x-face t
      gnus-treat-display-face t
      gnus-treat-display-smileys nil
      gnus-treat-x-pgp-sig 'head)

;; 设置邮件报头显示的信息
(setq gnus-visible-headers
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:"
                   "Organization:" "To:" "Cc:" "Followup-To" "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:" "X-Spam:" "X-Spam-Status:" "X-Now-Playing"
                   "X-Attachments" "X-Diagnostic" "X-RSS-URL")
                 "\\|"))

;; 设置邮件日期显示格式,使用两行日期，一行具体日期时间，
;; 另一行显示article, 距现在多长时间
(setq gnus-article-date-headers '(user-defined))
(setq gnus-article-time-format
      (lambda (time)
        (concat "X-Sent:   "
                (format-time-string "%Y年%m月%d日 星期%u %R" time)
                "\n"
                "X-Lasped: "
                (article-lapsed-string time))))

;; 用 Supercite 显示多种多样的引文形式
(setq sc-attrib-selection-list nil
      sc-auto-fill-region-p nil
      sc-blank-lines-after-headers 1
      sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
      sc-cite-blank-lines-p nil
      sc-confirm-always-p nil
      sc-electric-references-p nil
      sc-fixup-whitespace-p t
      sc-nested-citation-p nil
      sc-preferred-header-style 4
      sc-use-only-preference-p nil)

;; 构建 threads 时抓取旧文章标题,
;; 注意： 网速不快时不要使用这个选项。
(setq gnus-fetch-old-headers nil)

;; 聚集 threads 的方式
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Thread root 排序
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-number
        gnus-thread-sort-by-most-recent-date))

;; Subthread 排序
(setq gnus-subthread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date))

;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook
          #'gnus-group-first-unread-group)

(add-hook 'gnus-summary-exit-hook
          #'gnus-group-first-unread-group)


;; (setq mail-sources
;;       '((imap :server "imap.gmail.com"
;;               :user "xiongchenyu6@gmail.com"
;;               :port 993
;;               :stream ssl
;;               :fetchflag "\\Seen")))


