;;; info-occur.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.


;; Author: D. L. Nicolai <dalanicolaih@gmail.com>
;; Version: 20210522.1
;; Package-Requires: ((tablist "1.0"))
;; Keywords: help, matching
;; URL: https://github.com/dalanicolai/info-occur.el


;;; Commentary:

;; This package provides an Info-occur search feature, similar to pdf-occur.
;; When using the `Info-occur' command in an Info buffer, it will search in the
;; manual of that buffer. Using this command from other modes, it will ask to
;; select one or multiple manuals to search. To search in multiple manuals
;; directly from an info buffer prefix the command with a `universal-argument'.


;;; Code

;;; If it would work then this function should be used to add the text entry in
;;; the tablist vector (see Info-occur-tablist)
;; (defun info-occur-highlight-match (regexp)
;;   (let* ((text (string-replace "\n" "" (sentence-at-point)))
;;          (start (string-match regexp text))
;;          (end (match-end 0)))
;;     (add-face-text-property
;;      start
;;      end
;;      'match
;;      text)
;;     text))

(unless (require 'tablist nil t)
  (error "The info-occur package requires the tablist package to be available."))

(defun Info-occur-tablist (regexp &optional manuals)
  "MANUALS is a list of manuals names."
  (let (matches)
    (cond ((<= (length manuals) 1)
           (when manuals
             (info-display-manual (car manuals)))
           (Info-goto-node "Top")
           ;; Using condition-case because a _noerror argument is t has no effect
           (condition-case nil
               (while t
                 (Info-search regexp)
                 (let ((bm-record (bookmark-make-record)))
                   (push
                    (list
                     bm-record
                     (vector (alist-get 'info-node bm-record)
                             (string-replace "\n" "" (sentence-at-point))))
                             ;; (let* ((text (string-replace "\n" "" (sentence-at-point)))
                             ;;        (start (string-match regexp text))
                             ;;        (end (match-end 0)))
                             ;;   (add-face-text-property
                             ;;    start
                             ;;    end
                             ;;    'match
                             ;;    text)
                             ;;   text)))
                    matches)))
             (error nil)))
          (t
           (dolist (x manuals)
             (info-display-manual x)
             (Info-goto-node "Top")
             ;; Using condition-case because a _noerror argument is t has no effect
             (condition-case nil
                 (while t
                   (Info-search regexp)
                   (let ((bm-record (bookmark-make-record)))
                     (push
                      (list
                       bm-record
                       (vector x
                               (alist-get 'info-node bm-record)
                               (string-replace "\n" "" (sentence-at-point))))
                               ;; (let* ((text (string-replace "\n" "" (sentence-at-point)))
                               ;;        (start (string-match regexp text))
                               ;;        (end (match-end 0)))
                               ;;   (add-face-text-property
                               ;;    start
                               ;;    end
                               ;;    'match
                               ;;    text)
                               ;;   text)))
                      matches)))
               (error nil)))
           ))
    (nreverse matches)))

(define-derived-mode Info-occur-mode tablist-mode "Info-occur"
  "Major mode for browsing single Info manual search result"
  (setq-local tabulated-list-format [("Info-node" 30 nil) ("sentence" 80 nil)])
  (setq-local tablist-operations-function
              (lambda (op &rest _)
                (cl-case op
                  (supported-operations '(find-entry))
                  (find-entry (let ((item (tabulated-list-get-id)))
                                (bookmark--jump-via item 'switch-to-buffer-other-window))))))
  (tabulated-list-init-header))

(define-derived-mode Info-multi-occur-mode tablist-mode "Info-occur"
  "Major mode for browsing multiple Info manuals search result"
  (setq-local tabulated-list-format [("Manual" 20 nil) ("Info-node" 30 nil) ("sentence" 80 nil)])
  (setq-local tablist-operations-function
              (lambda (op &rest _)
                (cl-case op
                  (supported-operations '(find-entry))
                  (find-entry (let ((item (tabulated-list-get-id)))
                                (bookmark--jump-via item 'switch-to-buffer-other-window))))))
  (tabulated-list-init-header))

(defun Info-occur (regexp &optional arg)
  (interactive "sRegexp search: \nP")
  (let ((manuals (when (or arg
                           (not (eq major-mode 'Info-mode)))
                   (let (switch
                         manuals)
                     (while (not switch)
                       (push (completing-read "Add manual to search list: "
                                              (info--manual-names nil))
                             manuals)
                       (setq switch (not (y-or-n-p "Add-another-manual"))))
                     manuals))))
  ;; (info-assert-djvu-buffer)
    (let ((info-tablist (Info-occur-tablist regexp manuals)))
      (switch-to-buffer-other-window "Info-occur")
      (if (<= (length manuals) 1)
          (Info-occur-mode)
        (Info-multi-occur-mode))
      (setq-local tabulated-list-entries info-tablist)
      (tabulated-list-print))))
