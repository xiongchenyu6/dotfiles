;;; lsp-solidity.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 emacs-lsp maintainers

;; Author: emacs-lsp maintainers
;; Keywords: lsp, solidity, solidity

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP Clients for the Solidity Programming Language.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-solidity nil
  "LSP support for solidityl using solidity-language-server."
  :group 'lsp-mode
  :link '(url-link "https://www.npmjs.com/package/solidity-language-server"))

(defcustom lsp-clients-solidity-executable '("solidity-language-server"  "--stdio")
  "Command to start the solidity language server."
  :group 'lsp-solidity
  :risky t
  :type 'file)

(defcustom lsp-clients-solidity-initialization-options '()
  "Initialization options for solidity language server."
  :group 'lsp-solidity
  :type 'alist)

(lsp-dependency 'solidity-language-server
                '(:system "solidity-language-server")
                '(:npm :package "solidity-language-server"
                       :path "solidity-language-server"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find (cl-first lsp-clients-solidity-executable))
                                            (lsp-package-path 'solidity-language-server))
                                       ,@(cl-rest lsp-clients-solidity-executable))))
                  :major-modes '(solidity-mode)
                  :priority -1
                  :server-id 'solidity-language-server
                  :initialization-options (lambda () lsp-clients-solidity-initialization-options)
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'solidity-language-server
                                                            callback error-callback))))

(lsp-consistency-check lsp-solidity)

(provide 'lsp-solidity)
;;; lsp-solidity.el ends here
