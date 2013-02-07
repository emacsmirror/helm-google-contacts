;;; -*- coding: utf-8 mode: emacs-lisp -*-
;;; helm-google-contacts.el --- Search google contacts list by helm

;; Copyright (C) 2012 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Version: 0.0.1
;; Keywords: google, contact

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Requirements
;; This program is required google-contacts.el, you can install from
;; `http://stegard.net/tools/emacs/google-contacts.el'.
;; see also `http://stegard.net/tools/emacs/'

;;; Commentary:

;;; Usage:

(eval-when-compile (require 'cl))
(require 'google-contacts)
(require 'helm)

(defvar helm-google-contacts--concats-list '())
(defvar helm-google-contacts-for-name-and-email '())

(defvar helm-c-google-contacts
  '((name . "Google Contacts")
    (init . (lambda ()
              (unless helm-google-contacts-for-name-and-email
                (setq helm-google-contacts-for-name-and-email
                      (helm-google-contacts-extract-factor '(name emails))))))
    (candidates . (lambda ()
                    (loop for person in helm-google-contacts-for-name-and-email
                          collect (helm-google-contacts-perse person))))
    (candidate-number-limit . 1000)
    (nohighlight)
    (action . (lambda (line)
                (string-match "<\\(.+@.+\\)>" line)
                (insert (match-string 1 line))))
    (delayed)))

(defun helm-google-contacts-extract-factor (factor)
  "Specify symbol as emails, name, groups, phone-numbers to factor"
  (unless helm-google-contacts--concats-list
    (setq helm-google-contacts--concats-list (google-contacts-retrieve)))
  (lexical-let*
      ((return-mathced-factor
        (lambda (attr val)
          (loop for key in factor ; specify list as '(name emails)
                if (equal attr key)
                do (return `(,attr ,val)))))
       (extract-factor
        (lambda (personal)
          (loop with person = '()
                with tmp ='()
                for (attribute . value) in personal
                for match = (funcall return-mathced-factor attribute value)
                if match
                do (push match tmp)
                if (equal (length tmp) (length factor))
                do (setq person tmp)
                finally return person))))
    (loop for personal-information in helm-google-contacts--concats-list
          for each-other = (funcall extract-factor personal-information)
          if each-other
          collect each-other)))

(defun helm-google-contacts ()
  (interactive)
  (let ((helm-use-migemo t))
    (helm :sources helm-c-google-contacts
          :candidates-in-buffer t
          :buffer "*helm-google-contacts*")))

(defun helm-google-contacts-perse (person)
  (loop for (sym value) in person
        for name  = "" then name
        for email = "" then email
        do (typecase value
             (list ;; if value is list of emails
              (if (equal (length value) 1)
                  (setq email (caar value))))
             (string (setq name value)))
        finally return (concat name " <" email ">")))

(provide 'helm-google-contacts)
