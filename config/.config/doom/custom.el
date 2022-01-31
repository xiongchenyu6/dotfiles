(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((lsp-rust-analyzer-experimental-proc-attr-macros)
     (lsp-rust-analyzer-proc-macro-enable . t)
     (lsp-rust-analyzer-diagnostics-disabled .
                                             ["missing-unsafe"])
     (dap-debug-template-configurations
      ("cpptools::Run Configuration" :type "cppdbg" :request "launch" :name "cpptools::Run Configuration" :MIMode "lldb" :program "${workspaceFolder}/build/main" :cwd "${workspaceFolder}"))
     (lsp-rust-analyzer-experimental-proc-attr-macros . t)
     (projectile-project-compilation-cmd . "wasm-pack build"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
