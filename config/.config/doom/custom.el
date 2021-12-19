(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((checkdoc-package-keywords-flag)
     (dap-debug-template-configurations
      ("cpptools::Run Configuration" :type "cppdbg" :request "launch" :name "cpptools::Run Configuration" :MIMode "lldb" :program "${workspaceFolder}/build/main" :cwd "${workspaceFolder}"))
     (dap-debug-template-configurations
      ("cpptools::Run Configuration" :type "lldb" :request "launch" :name "cpptools::Run Configuration" :MIMode "lldb" :program "${workspaceFolder}/build/main" :cwd "${workspaceFolder}"))
     (firestarter-default-type quote failure)
     (firestarter . "nix-env -f '<nixpkgs>' -iA rEnv")
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (org-element-map . defun)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (->> . 1)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")))
 '(smtpmail-smtp-server "smtp.gmail.com" t)
 '(smtpmail-smtp-service 25 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
