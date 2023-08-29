(require 'cullers)

(deftheme monochwome "A monochromatic Emacs theme.")

(defgroup monochwome nil
  "Monochwome theme!"
  :group 'faces
  :prefix "monochwome"
  :tag "Monochwome Theme")

;;;###autoload
(defcustom monochwome-seed-hue "orange"
  "Hue for theme from which all other tones, tints, and shades will be derived.
This value should be a midtone value. Named colors like 'red', 'blue', 'green', and 'orange' look great!"
  :type 'color)

(defcustom monochwome-seed-to-bg-diff 80
  "How much darker the background color will be from the seed, in percent"
  :type 'number)

(defcustom monochwome-bg-range 30
  "How much darker the darkest background color is from the lightest background color."
  :type 'number)

(defcustom monochwome-fg-contrast-ratio 70
  "Contrast ratio between the lightest and darkest fg tones."
  :type 'number)

(defvar monochwome-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar monochwome-high-contrast-comments nil
  "When non-nil, increase contrast of comments and doc-strings.")

(defvar monochwome-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar monochwome-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom monochwome-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'monochwome-theme
  :package-version '(monochwome . "2.6"))

(defcustom monochwome-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'monochwome-theme
  :package-version '(monochwome . "2.6"))

(defcustom monochwome-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'monochwome-theme
  :package-version '(monochwome . "2.6"))

(defcustom monochwome-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'monochwome-theme
  :package-version '(monochwome . "2.6"))

(defcustom monochwome-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'monochwome-theme
  :package-version '(monochwome . "2.6"))

(defun monochwome-tints-from-colors (start-color stop-color steps)
  "Given a color, produce a range of tints. Color can be a name or hex. If hex, must start with a hash symbol.
Evaluates to a list of hex values."
  (let* ((start-rgb (color-name-to-rgb start-color))
	 (stop-rgb (color-name-to-rgb stop-color))
	 (gradient (color-gradient start-rgb stop-rgb steps)))
    (mapcar #'(lambda(color-step)
		(color-rgb-to-hex (nth 0 color-step)
				  (nth 1 color-step)
				  (nth 2 color-step) 2))
	    gradient)))

(setq monochwome--seed-hsl
      (culler-rgb-to-hsl (color-name-to-rgb monochwome-seed-hue)))

(setq monochwome-bg-0
      (culler-hsl-to-hex
       (culler-darken-hsl monochwome--seed-hsl
			  monochwome-seed-to-bg-diff)))

(setq monochwome--fg-color-darkest
      (culler-hsl-to-hex
       (culler-darken-hsl monochwome--seed-hsl
			  monochwome-fg-contrast-ratio)))

(setq monochwome--fg-color-lightest
      (culler-hsl-to-hex
       (culler-lighten-hsl monochwome--seed-hsl
			   monochwome-fg-contrast-ratio)))

(setq monochwome-tone-0-tints (monochwome-tints-from-colors
			       monochwome-seed-hue monochwome--fg-color-lightest 8))

(setq monochwome-tone-0-shades (monochwome-tints-from-colors
				monochwome-seed-hue monochwome--fg-color-darkest 8))

(setq monochwome-bg-darkest
      (culler-hsl-to-hex (culler-darken-hsl
			  (culler-rgb-to-hsl (color-name-to-rgb monochwome-bg-0))
			  (* monochwome-bg-range 100.0))))

(setq monochwome-bg-lightest
      (culler-hsl-to-hex (culler-lighten-hsl
			  (culler-rgb-to-hsl (color-name-to-rgb monochwome-bg-0))
			  (* monochwome-bg-range 100.0))))

(setq monochwome-bg-0-shades
      (append (monochwome-tints-from-colors monochwome-bg-0
					    monochwome-bg-darkest
					    3)
	      (list monochwome-bg-darkest)))

(setq monochwome-bg-0-tints
      (reverse (append (monochwome-tints-from-colors monochwome-bg-lightest
						     monochwome-bg-0
						     3)
		       (list monochwome-bg-lightest))))

;; There should be a way to set desired contrast ratios?? This theme works because of the phenomenon of contrast. So find
;; a way to let me control that, incase I want to change the contrast!

(defvar monochwome-default-colors-alist
  '(("monochwome-bg-3"     . (nth 3 monochwome-bg-0-shades))
	("monochwome-bg-2"     . (nth 2 monochwome-bg-0-shades))
	("monochwome-bg-1"     . (nth 1 monochwome-bg-0-shades))
	("monochwome-bg"       . monochwome-bg-0)
	("monochwome-bg+1"     . (nth 1 monochwome-bg-0-tints))
	("monochwome-bg+2"     . (nth 2 monochwome-bg-0-tints))
	("monochwome-bg+3"     . (nth 3 monochwome-bg-0-tints))
	("monochwome-tone-0+6" . (nth 6 monochwome-tone-0-tints))
	("monochwome-tone-0+5" . (nth 5 monochwome-tone-0-tints))
	("monochwome-tone-0+4" . (nth 4 monochwome-tone-0-tints))
	("monochwome-tone-0+3" . (nth 3 monochwome-tone-0-tints))
	("monochwome-tone-0+2" . (nth 2 monochwome-tone-0-tints))
	("monochwome-tone-0+3" . (nth 1 monochwome-tone-0-tints))
	("monochwome-tone-0+1" . (nth 0 monochwome-tone-0-tints))
	("monochwome-tone-0"   . monochwome-seed-hue)
	("monochwome-tone-0-1" . (nth 0 monochwome-tone-0-shades))
	("monochwome-tone-0-2" . (nth 1 monochwome-tone-0-shades))
	("monochwome-tone-0-3" . (nth 2 monochwome-tone-0-shades))
	("monochwome-tone-0-4" . (nth 3 monochwome-tone-0-shades))
	("monochwome-tone-0-5" . (nth 4 monochwome-tone-0-shades))
	("monochwome-lightest" . monochwome-bg-lightest)
	("monochwome-darkest"  . monochwome-bg-darkest)
	("monochwome-tone-1-6" . monochwome-seed-hue)
	("monochwome-tone-1-5" . monochwome-seed-hue)
	("monochwome-tone-1-4" . monochwome-seed-hue)
	("monochwome-tone-1-3" . monochwome-seed-hue)
	("monochwome-tone-1-2" . monochwome-seed-hue)
	("monochwome-tone-1-1" . monochwome-seed-hue)
	("monochwome-tone-1"   . monochwome-seed-hue)
	("monochwome-tone-1+1" . monochwome-seed-hue)
	("monochwome-tone-1+2" . monochwome-seed-hue)
	("monochwome-tone-2-2" . monochwome-seed-hue)
	("monochwome-tone-2-1" . monochwome-seed-hue)
	("monochwome-tone-2"   . monochwome-seed-hue)
	("monochwome-tone-3-5" . monochwome-seed-hue)
	("monochwome-tone-3-4" . monochwome-seed-hue)
	("monochwome-tone-3-3" . monochwome-seed-hue)
	("monochwome-tone-3-2" . monochwome-seed-hue)
	("monochwome-tone-3-1" . monochwome-seed-hue)
	("monochwome-tone-3"   . monochwome-seed-hue)
	("monochwome-tone-3+1" . monochwome-seed-hue)
	("monochwome-tone-3+2" . monochwome-seed-hue)
	("monochwome-tone-3+3" . monochwome-seed-hue)
	("monochwome-tone-3+4" . monochwome-seed-hue)
	("monochwome-tone-4+3" . monochwome-seed-hue)
	("monochwome-tone-4+2" . monochwome-seed-hue)
	("monochwome-tone-4+1" . monochwome-seed-hue)
	("monochwome-tone-4"   . monochwome-seed-hue)
	("monochwome-tone-4-1" . monochwome-seed-hue)
	("monochwome-tone-4-2" . monochwome-seed-hue)
	("monochwome-tone-4-3" . monochwome-seed-hue)
	("monochwome-tone-4-4" . monochwome-seed-hue)
	("monochwome-tone-4-5" . monochwome-seed-hue)
	("monochwome-tone-5"   . monochwome-seed-hue)
	("monochwome-tone-6"   . monochwome-seed-hue)
	("monochwome-tone-7"   . monochwome-seed-hue))
  "List of hues, tints, and shades for theme.
Each element has the form (NAME . HEX)

`+N' suffixes indicate a tint lighter.
`-N' suffixes indicate a tint darker.")

(defmacro monochwome-with-color-variables (&rest body)
  "`let bind all colors defined in `monochwome-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
	 ,@(mapcar (lambda (cons)
			 (list (intern (car cons)) (cdr cons)))
		   monochwome-default-colors-alist)
	 (monochwome-variable-pitch (if monochwome-use-variable-pitch
				   'variable-pitch 'default)))
	 ,@body))

;;; Theme Faces
(monochwome-with-color-variables
  (custom-theme-set-faces
   'monochwome

;;;;; basic coloring
   '(button         ((t (:underline t))))
   `(link           ((t (:foreground ,monochwome-tone-0 :underline t :italic t))))
   `(link-visited   ((t (:foreground ,monochwome-tone-0-2 :underline t :weight normal))))
   `(default        ((t (:foreground ,monochwome-tone-0+2 :background ,monochwome-bg :family "Iosevka" :weight light))))
   `(variable-pitch ((,class (:family "Iosevka" :weight light)))) ;; does this work?
   `(cursor         ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-0+1))))
   `(widget-field   ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg-0 :box (:line-width 1 :color ,monochwome-tone-0-5)))))
   `(escape-glyph   ((t (:foreground ,monochwome-tone-0 :weight bold))))
   `(fringe         ((t (:foreground ,monochwome-tone-0-4))))
   `(header-line    ((t (:foreground ,monochwome-tone-0
				  :background ,monochwome-bg-2
				  :box (:line-width -1 :style released-button)
				  :extend t))))
   `(highlight ((t (:background ,monochwome-bg+1))))
   `(success   ((t (:foreground ,monochwome-tone-3 :weight bold))))
   `(warning   ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(tooltip   ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg+1))))
;;;;; ansi-colors
   `(ansi-color-black ((t (:foreground ,monochwome-bg :background ,monochwome-bg-2))))
   `(ansi-color-red ((t (:foreground ,monochwome-tone-1-2 :background ,monochwome-tone-1-4))))
   `(ansi-color-green ((t (:foreground ,monochwome-tone-3 :background ,monochwome-tone-3+2))))
   `(ansi-color-yellow ((t (:foreground ,monochwome-tone-6 :background ,monochwome-tone-2))))
   `(ansi-color-blue ((t (:foreground ,monochwome-tone-4-1 :background ,monochwome-tone-4-4))))
   `(ansi-color-magenta ((t (:foreground ,monochwome-tone-7 :background ,monochwome-tone-1))))
   `(ansi-color-cyan ((t (:foreground ,monochwome-tone-5 :background ,monochwome-tone-4))))
   `(ansi-color-white ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-0-1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,monochwome-tone-2))))
   `(compilation-enter-directory-face ((t (:foreground ,monochwome-tone-3))))
   `(compilation-error-face ((t (:foreground ,monochwome-tone-1-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,monochwome-tone-0))))
   `(compilation-info-face ((t (:foreground ,monochwome-tone-4))))
   `(compilation-info ((t (:foreground ,monochwome-tone-3+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,monochwome-tone-3))))
   `(compilation-line-face ((t (:foreground ,monochwome-tone-2))))
   `(compilation-line-number ((t (:foreground ,monochwome-tone-2))))
   `(compilation-message-face ((t (:foreground ,monochwome-tone-4))))
   `(compilation-warning-face ((t (:foreground ,monochwome-tone-6 :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,monochwome-tone-3+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,monochwome-tone-2 :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,monochwome-tone-0-1))))
   `(completions-common-part ((t (:foreground ,monochwome-tone-4))))
   `(completions-first-difference ((t (:foreground ,monochwome-tone-0+1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,monochwome-tone-4 :weight bold))))
   `(custom-group-tag ((t (:foreground ,monochwome-tone-4 :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,monochwome-tone-3+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,monochwome-tone-0-5 :weight semilight)))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,monochwome-tone-0))))
   `(grep-error-face ((t (:foreground ,monochwome-tone-1-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,monochwome-tone-4))))
   `(grep-match-face ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(match ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-6 :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,monochwome-tone-5    :foreground ,monochwome-bg-2))))
   `(hi-green   ((t (:background ,monochwome-tone-3+4 :foreground ,monochwome-bg-2))))
   `(hi-pink    ((t (:background ,monochwome-tone-7 :foreground ,monochwome-bg-2))))
   `(hi-yellow  ((t (:background ,monochwome-tone-2  :foreground ,monochwome-bg-2))))
   `(hi-blue-b  ((t (:foreground ,monochwome-tone-4    :weight     bold))))
   `(hi-green-b ((t (:foreground ,monochwome-tone-3+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,monochwome-tone-1     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,monochwome-tone-0 :weight bold :background ,monochwome-bg+1))))
   `(isearch-fail ((t (:foreground ,monochwome-bg+1 :background ,monochwome-tone-0))))
   `(lazy-highlight ((t (:foreground ,monochwome-tone-0 :weight bold :background ,monochwome-bg))))

   `(menu ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(minibuffer-prompt ((t (:foreground ,monochwome-tone-0+1 :background ,monochwome-bg-0 :weight normal))))
;;;;; mode line
   `(mode-line ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0))))
   ;; `(mode-line-buffer-id ((t (:foreground ,monochwome-tone-0 :weight bold))))
   `(mode-line-inactive ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0-5))))

   `(region ((,class (:background ,monochwome-bg+1 :extend t))
		 (t :inverse-video t)))
   `(secondary-selection ((t (:background ,monochwome-tone-0+5))))
   `(trailing-whitespace ((t (:background ,monochwome-tone-1))))
   `(vertical-border ((t (:foreground ,monochwome-tone-0-5))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,monochwome-tone-0+3))))
   `(font-lock-comment-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0-3))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,monochwome-tone-0+4))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((t (:foreground ,monochwome-tone-0 :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,monochwome-tone-0-3 :underline t :weight light :width normal))))
   `(font-lock-negation-char-face ((t (:foreground ,monochwome-tone-0))))
   `(font-lock-preprocessor-face ((t (:foreground ,monochwome-tone-0))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,monochwome-tone-3 :weight bold))))
   `(font-lock-string-face ((t (:foreground ,monochwome-tone-0+2 :slant italic :family "Victor Mono")))) ;; should replace with var
   `(font-lock-type-face ((t (:foreground ,monochwome-tone-0+1 :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,monochwome-tone-0+2 :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,monochwome-tone-0+1 :underline t))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:inherit default :foreground ,monochwome-bg+3 :background ,monochwome-bg-1))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,monochwome-tone-2-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,monochwome-tone-0))))
   `(newsticker-default-face ((t (:foreground ,monochwome-tone-0))))
   `(newsticker-enclosure-face ((t (:foreground ,monochwome-tone-3+3))))
   `(newsticker-extra-face ((t (:foreground ,monochwome-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,monochwome-tone-0))))
   `(newsticker-immortal-item-face ((t (:foreground ,monochwome-tone-3))))
   `(newsticker-new-item-face ((t (:foreground ,monochwome-tone-4))))
   `(newsticker-obsolete-item-face ((t (:foreground ,monochwome-tone-1))))
   `(newsticker-old-item-face ((t (:foreground ,monochwome-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,monochwome-tone-0))))
   `(newsticker-treeview-face ((t (:foreground ,monochwome-tone-0))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,monochwome-tone-3))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,monochwome-tone-0))))
   `(newsticker-treeview-new-face ((t (:foreground ,monochwome-tone-4 :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,monochwome-tone-1))))
   `(newsticker-treeview-old-face ((t (:foreground ,monochwome-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-2))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))

;;;; Third-party packages

;;;;; ace-jump
   `(ace-jump-face-background
	 ((t (:foreground ,monochwome-tone-0-1 :background ,monochwome-bg :inverse-video nil))))
   `(ace-jump-face-foreground
	 ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
	 ((t (:foreground ,monochwome-tone-0-5 :background ,monochwome-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:foreground ,monochwome-tone-0+4 :weight bold))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,monochwome-tone-3+1))))
   `(android-mode-error-face ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(android-mode-info-face ((t (:foreground ,monochwome-tone-0))))
   `(android-mode-verbose-face ((t (:foreground ,monochwome-tone-3))))
   `(android-mode-warning-face ((t (:foreground ,monochwome-tone-2))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,monochwome-tone-5 :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(anzu-match-1 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-3))))
   `(anzu-match-2 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6))))
   `(anzu-match-3 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-4))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,monochwome-tone-2))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,monochwome-tone-1 :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,monochwome-tone-2))))
   `(font-latex-italic-face ((t (:foreground ,monochwome-tone-5 :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,monochwome-tone-6))))
   `(font-latex-script-char-face ((t (:foreground ,monochwome-tone-6))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,monochwome-tone-1))))
   `(agda2-highlight-symbol-face ((t (:foreground ,monochwome-tone-6))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,monochwome-tone-4-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,monochwome-tone-0))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,monochwome-tone-0))))
   `(agda2-highlight-datatype-face ((t (:foreground ,monochwome-tone-4))))
   `(agda2-highlight-function-face ((t (:foreground ,monochwome-tone-4))))
   `(agda2-highlight-module-face ((t (:foreground ,monochwome-tone-4-1))))
   `(agda2-highlight-error-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(agda2-highlight-typechecks-face ((t (:background ,monochwome-tone-1-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,monochwome-bg+3 :foreground ,monochwome-bg-3))))
   `(ac-selection-face ((t (:background ,monochwome-tone-4-4 :foreground ,monochwome-tone-0))))
   `(popup-tip-face ((t (:background ,monochwome-tone-2-2 :foreground ,monochwome-bg-3))))
   `(popup-menu-mouse-face ((t (:background ,monochwome-tone-2-2 :foreground ,monochwome-bg-3))))
   `(popup-summary-face ((t (:background ,monochwome-bg+3 :foreground ,monochwome-bg-3))))
   `(popup-scroll-bar-foreground-face ((t (:background ,monochwome-tone-4-5))))
   `(popup-scroll-bar-background-face ((t (:background ,monochwome-bg-2))))
   `(popup-isearch-match ((t (:background ,monochwome-bg :foreground ,monochwome-tone-0))))
;;;;; avy
   `(avy-background-face
	 ((t (:foreground ,monochwome-tone-0-1 :background ,monochwome-bg :inverse-video nil))))
   `(avy-lead-face-0
	 ((t (:foreground ,monochwome-tone-3+3 :background ,monochwome-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
	 ((t (:foreground ,monochwome-tone-2 :background ,monochwome-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
	 ((t (:foreground ,monochwome-tone-1+1 :background ,monochwome-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
	 ((t (:foreground ,monochwome-tone-5 :background ,monochwome-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg-3))))
   `(company-tooltip-annotation ((t (:foreground ,monochwome-tone-6 :background ,monochwome-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,monochwome-tone-6 :background ,monochwome-bg-2))))
   `(company-tooltip-selection ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg+1))))
   `(company-tooltip-mouse ((t (:background ,monochwome-bg-2))))
   `(company-tooltip-common ((t (:foreground ,monochwome-tone-3+2))))
   `(company-tooltip-common-selection ((t (:foreground ,monochwome-tone-3+2))))
   `(company-scrollbar-tint-0 ((t (:background ,monochwome-bg-2))))
   `(company-scrollbar-bg ((t (:background ,monochwome-bg+2))))
   `(company-preview ((t (:background ,monochwome-tone-3+2))))
   `(company-preview-common ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg-2))))
;;;;; bm
   `(bm-face ((t (:background ,monochwome-tone-2-1 :foreground ,monochwome-bg))))
   `(bm-fringe-face ((t (:background ,monochwome-tone-2-1 :foreground ,monochwome-bg))))
   `(bm-fringe-persistent-face ((t (:background ,monochwome-tone-3-2 :foreground ,monochwome-bg))))
   `(bm-persistent-face ((t (:background ,monochwome-tone-3-2 :foreground ,monochwome-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,monochwome-tone-1 :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,monochwome-tone-3))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,monochwome-tone-0-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,monochwome-tone-5))))
   `(cfw:face-saturday ((t (:foreground ,monochwome-tone-4 :weight bold))))
   `(cfw:face-select ((t (:background ,monochwome-tone-4-5))))
   `(cfw:face-sunday ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,monochwome-tone-5 :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,monochwome-tone-4-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,monochwome-bg :foreground ,monochwome-tone-0 :box nil))))
   `(centaur-tabs-selected ((t (:background ,monochwome-bg :foreground ,monochwome-tone-0+2 :box nil))))
   `(centaur-tabs-unselected ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-0-1 :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,monochwome-bg :foreground ,monochwome-tone-6 :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-6 :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,monochwome-tone-2 :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,monochwome-tone-2 :box nil))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground ,monochwome-tone-2 :box nil))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,monochwome-tone-6 :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,monochwome-tone-3+1))))
   `(cider-deprecated-face ((t (:background ,monochwome-tone-2-2))))
   `(cider-instrumented-face ((t (:box (:color ,monochwome-tone-1 :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,monochwome-tone-5 :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,monochwome-tone-1-4))))
   `(cider-test-error-face ((t (:background ,monochwome-tone-7))))
   `(cider-test-success-face ((t (:background ,monochwome-tone-3-2))))
   `(cider-fringe-good-face ((t (:foreground ,monochwome-tone-3+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,monochwome-tone-5))))
   `(circe-my-message-face ((t (:foreground ,monochwome-tone-0))))
   `(circe-fool-face ((t (:foreground ,monochwome-tone-1+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(circe-originator-face ((t (:foreground ,monochwome-tone-0))))
   `(circe-server-face ((t (:foreground ,monochwome-tone-3))))
   `(circe-topic-diff-new-face ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(circe-prompt-face ((t (:foreground ,monochwome-tone-6 :background ,monochwome-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,monochwome-tone-0)))
   `(context-coloring-level-1-face ((t :foreground ,monochwome-tone-5)))
   `(context-coloring-level-2-face ((t :foreground ,monochwome-tone-3+4)))
   `(context-coloring-level-3-face ((t :foreground ,monochwome-tone-2)))
   `(context-coloring-level-4-face ((t :foreground ,monochwome-tone-6)))
   `(context-coloring-level-5-face ((t :foreground ,monochwome-tone-7)))
   `(context-coloring-level-6-face ((t :foreground ,monochwome-tone-4+1)))
   `(context-coloring-level-7-face ((t :foreground ,monochwome-tone-3+2)))
   `(context-coloring-level-8-face ((t :foreground ,monochwome-tone-2-2)))
   `(context-coloring-level-9-face ((t :foreground ,monochwome-tone-1+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,monochwome-tone-4 :foreground ,monochwome-bg))))
   `(ctbl:face-continue-bar ((t (:background ,monochwome-bg-1 :foreground ,monochwome-bg))))
   `(ctbl:face-row-select ((t (:background ,monochwome-tone-5 :foreground ,monochwome-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,monochwome-tone-0-1))))
   `(debbugs-gnu-handled ((t (:foreground ,monochwome-tone-3))))
   `(debbugs-gnu-new ((t (:foreground ,monochwome-tone-1))))
   `(debbugs-gnu-pending ((t (:foreground ,monochwome-tone-4))))
   `(debbugs-gnu-stale ((t (:foreground ,monochwome-tone-6))))
   `(debbugs-gnu-tagged ((t (:foreground ,monochwome-tone-1))))
;;;;; diff
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(diff-added          ((t (:background "#335533" :foreground ,monochwome-tone-3))))
   `(diff-changed        ((t (:background "#555511" :foreground ,monochwome-tone-2-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,monochwome-tone-1-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,monochwome-tone-3+4))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,monochwome-tone-2))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,monochwome-tone-1))))
   `(diff-header ((,class (:background ,monochwome-bg+2))
		  (t (:background ,monochwome-tone-0 :foreground ,monochwome-bg))))
   `(diff-file-header
	 ((,class (:background ,monochwome-bg+2 :foreground ,monochwome-tone-0 :weight bold))
	  (t (:background ,monochwome-tone-0 :foreground ,monochwome-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-margin-symbols-alist '((insert . "+") (delete . "-") (change . "!") (unknown . "?") (ignored . "i")))
   `(diff-hl-change ((,class (:foreground ,monochwome-tone-4 :background ,monochwome-bg))))
   `(diff-hl-delete ((,class (:foreground ,monochwome-tone-1+1 :background ,monochwome-bg))))
   `(diff-hl-insert ((,class (:foreground ,monochwome-tone-3+1 :background ,monochwome-bg))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,monochwome-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,monochwome-tone-0-4))))
   `(diredp-compressed-file-suffix ((t (:foreground ,monochwome-tone-6))))
   `(diredp-date-time ((t (:foreground ,monochwome-tone-0-3))))
   `(diredp-deletion ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0+5))))
   `(diredp-deletion-file-name ((t (:foreground ,monochwome-tone-1))))
   `(diredp-dir-heading ((t (:foreground ,monochwome-tone-4 :background ,monochwome-bg-2))))
   `(diredp-dir-priv ((t (:foreground ,monochwome-tone-5))))
   `(diredp-exec-priv ((t (:foreground ,monochwome-tone-1))))
   `(diredp-executable-tag ((t (:foreground ,monochwome-tone-3+1))))
   `(diredp-file-name ((t (:foreground ,monochwome-tone-0+4))))
   `(diredp-file-suffix ((t (:foreground ,monochwome-tone-0-3))))
   `(diredp-flag-mark ((t (:foreground ,monochwome-tone-2))))
   `(diredp-flag-mark-line ((t (:foreground ,monochwome-tone-6))))
   `(diredp-ignored-file-name ((t (:foreground ,monochwome-tone-1))))
   `(diredp-link-priv ((t (:foreground ,monochwome-tone-2))))
   `(diredp-mode-line-flagged ((t (:foreground ,monochwome-tone-2))))
   `(diredp-mode-line-marked ((t (:foreground ,monochwome-tone-6))))
   `(diredp-no-priv ((t (:foreground ,monochwome-tone-0))))
   `(diredp-number ((t (:foreground ,monochwome-tone-3+1))))
   `(diredp-other-priv ((t (:foreground ,monochwome-tone-2-1))))
   `(diredp-rare-priv ((t (:foreground ,monochwome-tone-1-1))))
   `(diredp-read-priv ((t (:foreground ,monochwome-tone-3-2))))
   `(diredp-symlink ((t (:foreground ,monochwome-tone-2))))
   `(diredp-write-priv ((t (:foreground ,monochwome-tone-7))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(dired-async-message ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,monochwome-tone-2))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,monochwome-tone-6))))
   `(diredfl-date-time ((t (:foreground ,monochwome-tone-7))))
   `(diredfl-deletion ((t (:foreground ,monochwome-tone-2))))
   `(diredfl-deletion-file-name ((t (:foreground ,monochwome-tone-1))))
   `(diredfl-dir-heading ((t (:foreground ,monochwome-tone-4 :background ,monochwome-bg-2))))
   `(diredfl-dir-priv ((t (:foreground ,monochwome-tone-5))))
   `(diredfl-exec-priv ((t (:foreground ,monochwome-tone-1))))
   `(diredfl-executable-tag ((t (:foreground ,monochwome-tone-3+1))))
   `(diredfl-file-name ((t (:foreground ,monochwome-tone-4))))
   `(diredfl-file-suffix ((t (:foreground ,monochwome-tone-3))))
   `(diredfl-flag-mark ((t (:foreground ,monochwome-tone-2))))
   `(diredfl-flag-mark-line ((t (:foreground ,monochwome-tone-6))))
   `(diredfl-ignored-file-name ((t (:foreground ,monochwome-tone-1))))
   `(diredfl-link-priv ((t (:foreground ,monochwome-tone-2))))
   `(diredfl-no-priv ((t (:foreground ,monochwome-tone-0))))
   `(diredfl-number ((t (:foreground ,monochwome-tone-3+1))))
   `(diredfl-other-priv ((t (:foreground ,monochwome-tone-2-1))))
   `(diredfl-rare-priv ((t (:foreground ,monochwome-tone-1-1))))
   `(diredfl-read-priv ((t (:foreground ,monochwome-tone-3-1))))
   `(diredfl-symlink ((t (:foreground ,monochwome-tone-2))))
   `(diredfl-write-priv ((t (:foreground ,monochwome-tone-7))))
;;;;; doom-modeline
   `(doom-modeline-bar  ((t (:background ,monochwome-tone-2))))
   `(doom-modeline-inactive-bar  ((t (:background nil))))
   `(doom-modeline-project-dir ((t (:inherit mone-line))))
   `(doom-modeline-buffer-modified ((t (:inherit mone-line :underline t))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-1-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-1-4))))
   `(ediff-current-diff-B ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-3-2))))
   `(ediff-current-diff-C ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-4-5))))
   `(ediff-even-diff-A ((t (:background ,monochwome-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,monochwome-bg+1))))
   `(ediff-even-diff-B ((t (:background ,monochwome-bg+1))))
   `(ediff-even-diff-C ((t (:background ,monochwome-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-1-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-1-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-3 :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-4-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,monochwome-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,monochwome-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,monochwome-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,monochwome-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,monochwome-tone-0))))
   `(egg-help-header-1 ((t (:foreground ,monochwome-tone-2))))
   `(egg-help-header-2 ((t (:foreground ,monochwome-tone-3+3))))
   `(egg-branch ((t (:foreground ,monochwome-tone-2))))
   `(egg-branch-mono ((t (:foreground ,monochwome-tone-2))))
   `(egg-term ((t (:foreground ,monochwome-tone-2))))
   `(egg-diff-add ((t (:foreground ,monochwome-tone-3+4))))
   `(egg-diff-del ((t (:foreground ,monochwome-tone-1+1))))
   `(egg-diff-file-header ((t (:foreground ,monochwome-tone-2-2))))
   `(egg-section-title ((t (:foreground ,monochwome-tone-2))))
   `(egg-stash-mono ((t (:foreground ,monochwome-tone-3+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,monochwome-tone-1))))
   `(elfeed-log-info-level-face ((t (:foreground ,monochwome-tone-4))))
   `(elfeed-log-warn-level-face ((t (:foreground ,monochwome-tone-2))))
   `(elfeed-search-date-face ((t (:foreground ,monochwome-tone-2-1 :underline t
						  :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,monochwome-tone-3))))
   `(elfeed-search-feed-face ((t (:foreground ,monochwome-tone-5))))
   `(elfeed-search-title-face ((t (:foreground ,monochwome-tone-0-1))))
   `(elfeed-search-unread-title-face ((t (:foreground ,monochwome-tone-0 :weight bold))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,monochwome-tone-2 :underline t
				 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,monochwome-tone-2-2
					 :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,monochwome-tone-1-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,monochwome-tone-2
							 :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg))))
   `(w3m-lnum-match ((t (:background ,monochwome-bg-2
					 :foreground ,monochwome-tone-6
					 :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,monochwome-tone-2))))
;;;;; emms
   `(emms-playlist-track-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
   `(emms-playlist-selected-face ((t (:inherit ,monochwome-variable-pitch :weight bold :foreground ,monochwome-tone-0+3))))
   `(emms-browser-year/genre-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
   `(emms-browser-artist-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
   `(emms-browser-composer-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
   `(emms-browser-performer-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
   `(emms-browser-album-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
   `(emms-browser-track-face ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,monochwome-tone-4 :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,monochwome-tone-0))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,monochwome-tone-2))))
   `(erc-keyword-face ((t (:foreground ,monochwome-tone-4 :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,monochwome-tone-3))))
   `(erc-pal-face ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(erc-prompt-face ((t (:foreground ,monochwome-tone-6 :background ,monochwome-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,monochwome-tone-3+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,monochwome-tone-3+4 :background ,monochwome-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,monochwome-tone-1 :background ,monochwome-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,monochwome-tone-1-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,monochwome-tone-4+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,monochwome-tone-1+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,monochwome-tone-0))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,monochwome-tone-5 :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,monochwome-tone-3+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-1-1) :inherit unspecified))
	  (t (:foreground ,monochwome-tone-1-1 :weight bold :underline t))))
   `(flycheck-warning
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-2) :inherit unspecified))
	  (t (:foreground ,monochwome-tone-2 :weight bold :underline t))))
   `(flycheck-info
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-5) :inherit unspecified))
	  (t (:foreground ,monochwome-tone-5 :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,monochwome-tone-1-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,monochwome-tone-5 :weight bold))))
;;;;; flymake
   `(flymake-errline
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-1)
		   :inherit unspecified :foreground unspecified :background unspecified))
	  (t (:foreground ,monochwome-tone-1-1 :weight bold :underline t))))
   `(flymake-warnline
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6)
		   :inherit unspecified :foreground unspecified :background unspecified))
	  (t (:foreground ,monochwome-tone-6 :weight bold :underline t))))
   `(flymake-infoline
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-3)
		   :inherit unspecified :foreground unspecified :background unspecified))
	  (t (:foreground ,monochwome-tone-3-2 :weight bold :underline t))))
   `(flymake-error
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-1)
		   :inherit unspecified :foreground unspecified :background unspecified))
	  (t (:foreground ,monochwome-tone-1-1 :weight bold :underline t))))
   `(flymake-warning
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6)
		   :inherit unspecified :foreground unspecified :background unspecified))
	  (t (:foreground ,monochwome-tone-6 :weight bold :underline t))))
   `(flymake-note
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-3)
		   :inherit unspecified :foreground unspecified :background unspecified))
	  (t (:foreground ,monochwome-tone-3-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6) :inherit unspecified))
	  (t (:foreground ,monochwome-tone-6 :weight bold :underline t))))
   `(flyspell-incorrect
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6) :inherit unspecified))
	  (t (:foreground ,monochwome-tone-1-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,monochwome-tone-0))))
   `(ack-file ((t (:foreground ,monochwome-tone-4))))
   `(ack-line ((t (:foreground ,monochwome-tone-2))))
   `(ack-match ((t (:foreground ,monochwome-tone-6 :background ,monochwome-bg-2 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,monochwome-tone-3+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,monochwome-tone-4+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,monochwome-tone-4+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,monochwome-tone-3  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,monochwome-tone-2  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,monochwome-tone-3 :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,monochwome-tone-1 :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,monochwome-tone-7 :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,monochwome-tone-0 :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,monochwome-tone-3  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,monochwome-tone-7 :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, monochwome-tone-6))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,monochwome-tone-3+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,monochwome-tone-1+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,monochwome-tone-4 :slant italic))))
   `(gnus-server-offline ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(gnus-server-agent ((t (:foreground ,monochwome-tone-4 :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,monochwome-tone-6))))
   `(gnus-summary-high-ancient ((t (:foreground ,monochwome-tone-4))))
   `(gnus-summary-high-read ((t (:foreground ,monochwome-tone-3 :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,monochwome-tone-0 :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,monochwome-tone-4))))
   `(gnus-summary-low-read ((t (:foreground ,monochwome-tone-3))))
   `(gnus-summary-low-ticked ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,monochwome-tone-0))))
   `(gnus-summary-normal-ancient ((t (:foreground ,monochwome-tone-4))))
   `(gnus-summary-normal-read ((t (:foreground ,monochwome-tone-3))))
   `(gnus-summary-normal-ticked ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,monochwome-tone-0))))
   `(gnus-summary-selected ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,monochwome-tone-4))))
   `(gnus-cite-10 ((t (:foreground ,monochwome-tone-2-1))))
   `(gnus-cite-11 ((t (:foreground ,monochwome-tone-2))))
   `(gnus-cite-2 ((t (:foreground ,monochwome-tone-4-1))))
   `(gnus-cite-3 ((t (:foreground ,monochwome-tone-4-2))))
   `(gnus-cite-4 ((t (:foreground ,monochwome-tone-3+2))))
   `(gnus-cite-5 ((t (:foreground ,monochwome-tone-3+1))))
   `(gnus-cite-6 ((t (:foreground ,monochwome-tone-3))))
   `(gnus-cite-7 ((t (:foreground ,monochwome-tone-1))))
   `(gnus-cite-8 ((t (:foreground ,monochwome-tone-1-1))))
   `(gnus-cite-9 ((t (:foreground ,monochwome-tone-1-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,monochwome-tone-2))))
   `(gnus-group-news-2-empty ((t (:foreground ,monochwome-tone-3+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,monochwome-tone-3+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,monochwome-tone-4-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,monochwome-tone-4-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,monochwome-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,monochwome-bg+2))))
   `(gnus-signature ((t (:foreground ,monochwome-tone-2))))
   `(gnus-x ((t (:background ,monochwome-tone-0 :foreground ,monochwome-bg))))
   `(mm-uu-extract ((t (:background ,monochwome-bg-1 :foreground ,monochwome-tone-3+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,monochwome-bg-2 :background ,monochwome-tone-3+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,monochwome-tone-4))))
   `(guide-key/key-face ((t (:foreground ,monochwome-tone-3))))
   `(guide-key/prefix-command-face ((t (:foreground ,monochwome-tone-3+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,monochwome-bg-2
		      :background ,monochwome-tone-0-5
		      :underline nil
		      :box nil
		      :extend t))))
   `(helm-source-header
     ((t (:foreground ,monochwome-tone-2
		      :foreground ,monochwome-tone-0+5
		      :box (:line-width 1 :color ,monochwome-tone-0-5)
		      :underline nil
		      :extend t))))
   `(helm-selection ((t (:background ,monochwome-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,monochwome-bg+1))))
   `(helm-visible-mark ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0-2))))
   `(helm-candidate-number ((t (:foreground ,monochwome-tone-0+6 :background ,monochwome-bg-3))))
   `(helm-separator ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-time-zone-current ((t (:foreground ,monochwome-tone-0+2 :background ,monochwome-bg))))
   `(helm-time-zone-home ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-bookmark-info ((t (:foreground ,monochwome-tone-0+2 :background ,monochwome-bg))))
   `(helm-bookmark-man ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-buffer-directory ((t (:weight bold :foreground ,monochwome-tone-0 :background ,monochwome-bg-0))))
   `(helm-buffer-not-saved ((t (:underline t))))
   `(helm-buffer-modified ((t (:underline t))))
   `(helm-buffer-process ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-buffer-size ((t (:foreground ,monochwome-tone-0-1 :background ,monochwome-bg))))
   `(helm-ff-directory ((t (:foreground ,monochwome-tone-0+3 :background ,monochwome-bg :weight bold :underline t))))
   `(helm-ff-file ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg :weight light))))
   `(helm-ff-file-extension ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,monochwome-tone-0+2 :background ,monochwome-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0 :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-grep-file ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-grep-finish ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-grep-lineno ((t (:foreground ,monochwome-tone-0-1 :background ,monochwome-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-match ((t (:background ,monochwome-bg+3))))
   `(helm-moccur-buffer ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,monochwome-tone-0-1 :background ,monochwome-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg))))
;;;;; helm-lxc
   `(helm-lxc-face-frozen ((t (:foreground ,monochwome-tone-4 :background ,monochwome-bg))))
   `(helm-lxc-face-running ((t (:foreground ,monochwome-tone-3 :background ,monochwome-bg))))
   `(helm-lxc-face-stopped ((t (:foreground ,monochwome-tone-1 :background ,monochwome-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,monochwome-tone-2 :background ,monochwome-bg+2 :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,monochwome-tone-4))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,monochwome-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((t (:background ,monochwome-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,monochwome-bg+1))
		   (t :weight bold)))
   `(hl-line ((,class (:background ,monochwome-bg+1 :extend t)) ; old emacsen
		  (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,monochwome-bg+1))
		   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,monochwome-tone-1-1 :background ,monochwome-bg))))
   `(hydra-face-amaranth ((t (:foreground ,monochwome-tone-1-3 :background ,monochwome-bg))))
   `(hydra-face-blue ((t (:foreground ,monochwome-tone-4 :background ,monochwome-bg))))
   `(hydra-face-pink ((t (:foreground ,monochwome-tone-7 :background ,monochwome-bg))))
   `(hydra-face-teal ((t (:foreground ,monochwome-tone-5 :background ,monochwome-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-6))))
   `(info-constant-ref-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-7))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-2))))
   `(info-function-ref-item ((t (:background ,monochwome-bg-2 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-2))))
   `(info-menu ((t (:foreground ,monochwome-tone-2))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,monochwome-bg-2))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-2))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-4+1))))
   `(info-user-option-ref-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-1))))
   `(info-variable-ref-item ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-6))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(irfc-reference-face ((t (:foreground ,monochwome-tone-4-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,monochwome-tone-5 :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,monochwome-tone-3+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,monochwome-tone-3+3))))
   `(irfc-title-face ((t (:foreground ,monochwome-tone-2
					  :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,monochwome-tone-3 :background ,monochwome-bg))))
   `(ivy-current-match ((t (:foreground ,monochwome-tone-2 :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0))))
   `(ivy-match-required-face ((t (:foreground ,monochwome-tone-1 :background ,monochwome-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,monochwome-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,monochwome-tone-3-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,monochwome-tone-3))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,monochwome-tone-3+1))))
   `(ivy-remote ((t (:foreground ,monochwome-tone-4 :background ,monochwome-bg))))
   `(ivy-subdir ((t (:foreground ,monochwome-tone-2 :background ,monochwome-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(ido-only-match ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(ido-subdir ((t (:foreground ,monochwome-tone-2))))
   `(ido-indicator ((t (:foreground ,monochwome-tone-2 :background ,monochwome-tone-1-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,monochwome-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,monochwome-tone-3+2))))
   `(jabber-roster-user-online ((t (:foreground ,monochwome-tone-4-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,monochwome-tone-1+1))))
   `(jabber-roster-user-xa ((t (:foreground ,monochwome-tone-7))))
   `(jabber-roster-user-chatty ((t (:foreground ,monochwome-tone-6))))
   `(jabber-roster-user-error ((t (:foreground ,monochwome-tone-1+1))))
   `(jabber-rare-time-face ((t (:foreground ,monochwome-tone-3+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,monochwome-tone-4-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,monochwome-tone-1+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,monochwome-tone-3+3))))
   `(jabber-activity-face((t (:foreground ,monochwome-tone-1+1))))
   `(jabber-activity-personal-face ((t (:foreground ,monochwome-tone-4+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,monochwome-tone-6))))
   `(js2-error ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,monochwome-tone-3-2))))
   `(js2-jsdoc-type ((t (:foreground ,monochwome-tone-3+2))))
   `(js2-jsdoc-value ((t (:foreground ,monochwome-tone-3+3))))
   `(js2-function-param ((t (:foreground, monochwome-tone-6))))
   `(js2-external-variable ((t (:foreground ,monochwome-tone-6))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,monochwome-tone-3-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,monochwome-tone-6))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,monochwome-tone-1-1))))
   `(js2-object-property ((t (:foreground ,monochwome-tone-4+1))))
   `(js2-magic-paren ((t (:foreground ,monochwome-tone-4-5))))
   `(js2-private-function-call ((t (:foreground ,monochwome-tone-5))))
   `(js2-function-call ((t (:foreground ,monochwome-tone-5))))
   `(js2-private-member ((t (:foreground ,monochwome-tone-4-1))))
   `(js2-keywords ((t (:foreground ,monochwome-tone-7))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,monochwome-tone-1-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,monochwome-tone-0 :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,monochwome-tone-1 :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,monochwome-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,monochwome-tone-2-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,monochwome-tone-3 :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,monochwome-tone-6 weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,monochwome-tone-0))))
   `(ledger-font-posting-date-face ((t (:foreground ,monochwome-tone-6 :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,monochwome-tone-4-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,monochwome-tone-0))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,monochwome-tone-6))))
   `(ledger-font-posting-amount-face ((t (:foreground ,monochwome-tone-6))))
   `(ledger-occur-narrowed-face ((t (:foreground ,monochwome-tone-0-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,monochwome-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,monochwome-tone-3))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,monochwome-tone-1-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,monochwome-tone-0 :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,monochwome-tone-6 :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,monochwome-tone-6 :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,monochwome-bg-1 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-0))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,monochwome-tone-2))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,monochwome-tone-0))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,monochwome-tone-2))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,monochwome-tone-2 :box t))))
   `(ruler-mode-default ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg))))

;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,monochwome-tone-4-1))))
   `(lui-hilight-face ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
	 ((t (:foreground ,monochwome-tone-3+2 :background ,monochwome-bg-2))))
   `(macrostep-gensym-2
	 ((t (:foreground ,monochwome-tone-1+1 :background ,monochwome-bg-2))))
   `(macrostep-gensym-3
	 ((t (:foreground ,monochwome-tone-4+1 :background ,monochwome-bg-2))))
   `(macrostep-gensym-4
	 ((t (:foreground ,monochwome-tone-7 :background ,monochwome-bg-2))))
   `(macrostep-gensym-5
	 ((t (:foreground ,monochwome-tone-2 :background ,monochwome-bg-2))))
   `(macrostep-expansion-highlight-face
	 ((t (:inherit highlight))))
   `(macrostep-macro-face
	 ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   ;; Please read (info "(magit)Theming Faces") before changing this.
   `(magit-section-highlight           ((t (:background ,monochwome-bg+1))))
   `(magit-section-heading             ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,monochwome-bg+1 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,monochwome-bg+1 :weight bold
							:foreground ,monochwome-tone-6))))
   ;;`(magit-diff-added                  ((t (:background ,monochwome-tone-3-2))))
   ;;`(magit-diff-added-highlight        ((t (:background ,monochwome-tone-3))))
   ;;`(magit-diff-removed                ((t (:background ,monochwome-tone-1-4))))
   ;;`(magit-diff-removed-highlight      ((t (:background ,monochwome-tone-1-3))))
   `(magit-diff-hunk-heading           ((t (:background ,monochwome-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,monochwome-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,monochwome-bg+2
							:foreground ,monochwome-tone-6))))
   `(magit-diff-lines-heading          ((t (:background ,monochwome-tone-6
							:foreground ,monochwome-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,monochwome-bg+1
							:foreground "grey70"))))
   `(magit-diffstat-added              ((t (:foreground ,monochwome-tone-3+4))))
   `(magit-diffstat-removed            ((t (:foreground ,monochwome-tone-1))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,monochwome-tone-2  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,monochwome-tone-3-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,monochwome-tone-3   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,monochwome-tone-0-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,monochwome-tone-4-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,monochwome-tone-3  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,monochwome-tone-1    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,monochwome-tone-6))))
   `(magit-log-date      ((t (:foreground ,monochwome-tone-0-1))))
   `(magit-log-graph     ((t (:foreground ,monochwome-tone-0+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,monochwome-tone-2-2))))
   `(magit-sequence-stop ((t (:foreground ,monochwome-tone-3))))
   `(magit-sequence-part ((t (:foreground ,monochwome-tone-2))))
   `(magit-sequence-head ((t (:foreground ,monochwome-tone-4))))
   `(magit-sequence-drop ((t (:foreground ,monochwome-tone-1))))
   `(magit-sequence-done ((t (:foreground ,monochwome-tone-0-1))))
   `(magit-sequence-onto ((t (:foreground ,monochwome-tone-0-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,monochwome-tone-3))))
   `(magit-bisect-skip ((t (:foreground ,monochwome-tone-2))))
   `(magit-bisect-bad  ((t (:foreground ,monochwome-tone-1))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-4-2))))
   `(magit-blame-hash    ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-4-2))))
   `(magit-blame-name    ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-6))))
   `(magit-blame-date    ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-6))))
   `(magit-blame-summary ((t (:background ,monochwome-bg-2 :foreground ,monochwome-tone-4-2
					  :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,monochwome-bg+3))))
   `(magit-hash           ((t (:foreground ,monochwome-bg+3))))
   `(magit-tag            ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,monochwome-tone-3  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,monochwome-tone-4   :weight bold))))
   `(magit-branch-current ((t (:foreground ,monochwome-tone-4   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,monochwome-tone-4   :weight bold))))
   `(magit-refname        ((t (:background ,monochwome-bg+2 :foreground ,monochwome-tone-0 :weight bold))))
   `(magit-refname-stash  ((t (:background ,monochwome-bg+2 :foreground ,monochwome-tone-0 :weight bold))))
   `(magit-refname-wip    ((t (:background ,monochwome-bg+2 :foreground ,monochwome-tone-0 :weight bold))))
   `(magit-signature-good      ((t (:foreground ,monochwome-tone-3))))
   `(magit-signature-bad       ((t (:foreground ,monochwome-tone-1))))
   `(magit-signature-untrusted ((t (:foreground ,monochwome-tone-2))))
   `(magit-signature-expired   ((t (:foreground ,monochwome-tone-6))))
   `(magit-signature-revoked   ((t (:foreground ,monochwome-tone-7))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,monochwome-tone-5))))
   `(magit-cherry-equivalent   ((t (:foreground ,monochwome-tone-7))))
   `(magit-reflog-commit       ((t (:foreground ,monochwome-tone-3))))
   `(magit-reflog-amend        ((t (:foreground ,monochwome-tone-7))))
   `(magit-reflog-merge        ((t (:foreground ,monochwome-tone-3))))
   `(magit-reflog-checkout     ((t (:foreground ,monochwome-tone-4))))
   `(magit-reflog-reset        ((t (:foreground ,monochwome-tone-1))))
   `(magit-reflog-rebase       ((t (:foreground ,monochwome-tone-7))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,monochwome-tone-3))))
   `(magit-reflog-remote       ((t (:foreground ,monochwome-tone-5))))
   `(magit-reflog-other        ((t (:foreground ,monochwome-tone-5))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,monochwome-tone-4+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,monochwome-tone-2))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,monochwome-tone-2-2 :underline t))))
   `(markup-list-face ((t (:foreground ,monochwome-tone-0+1))))
   `(markup-meta-face ((t (:foreground ,monochwome-tone-2))))
   `(markup-meta-hide-face ((t (:foreground ,monochwome-tone-2))))
   `(markup-secondary-text-face ((t (:foreground ,monochwome-tone-2-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,monochwome-tone-2))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,monochwome-tone-3+1))))
   `(message-header-other ((t (:foreground ,monochwome-tone-3))))
   `(message-header-to ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(message-header-cc ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(message-header-subject ((t (:foreground ,monochwome-tone-6 :weight bold))))
   `(message-header-xheader ((t (:foreground ,monochwome-tone-3))))
   `(message-mml ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,monochwome-tone-6))))
   `(mew-face-header-from ((t (:foreground ,monochwome-tone-2))))
   `(mew-face-header-date ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-header-to ((t (:foreground ,monochwome-tone-1))))
   `(mew-face-header-key ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-header-private ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-header-important ((t (:foreground ,monochwome-tone-4))))
   `(mew-face-header-marginal ((t (:foreground ,monochwome-tone-0 :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,monochwome-tone-1))))
   `(mew-face-header-xmew ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-header-xmew-bad ((t (:foreground ,monochwome-tone-1))))
   `(mew-face-body-url ((t (:foreground ,monochwome-tone-6))))
   `(mew-face-body-comment ((t (:foreground ,monochwome-tone-0 :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-body-cite2 ((t (:foreground ,monochwome-tone-4))))
   `(mew-face-body-cite3 ((t (:foreground ,monochwome-tone-6))))
   `(mew-face-body-cite4 ((t (:foreground ,monochwome-tone-2))))
   `(mew-face-body-cite5 ((t (:foreground ,monochwome-tone-1))))
   `(mew-face-mark-review ((t (:foreground ,monochwome-tone-4))))
   `(mew-face-mark-escape ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-mark-delete ((t (:foreground ,monochwome-tone-1))))
   `(mew-face-mark-unlink ((t (:foreground ,monochwome-tone-2))))
   `(mew-face-mark-refile ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-mark-unread ((t (:foreground ,monochwome-tone-1-2))))
   `(mew-face-eof-message ((t (:foreground ,monochwome-tone-3))))
   `(mew-face-eof-part ((t (:foreground ,monochwome-tone-2))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,monochwome-tone-5 :background ,monochwome-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7 :weight bold))))
   `(paren-face-no-match ((t (:foreground ,monochwome-bg :background ,monochwome-tone-1 :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,monochwome-tone-4))))
   `(mingus-pausing-face ((t (:foreground ,monochwome-tone-7))))
   `(mingus-playing-face ((t (:foreground ,monochwome-tone-5))))
   `(mingus-playlist-face ((t (:foreground ,monochwome-tone-5 ))))
   `(mingus-mark-face ((t (:bold t :foreground ,monochwome-tone-7))))
   `(mingus-song-file-face ((t (:foreground ,monochwome-tone-2))))
   `(mingus-artist-face ((t (:foreground ,monochwome-tone-5))))
   `(mingus-album-face ((t (:underline t :foreground ,monochwome-tone-1+1))))
   `(mingus-album-stale-face ((t (:foreground ,monochwome-tone-1+1))))
   `(mingus-stopped-face ((t (:foreground ,monochwome-tone-1))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,monochwome-tone-2))))
   `(nav-face-button-num ((t (:foreground ,monochwome-tone-5))))
   `(nav-face-dir ((t (:foreground ,monochwome-tone-3))))
   `(nav-face-hdir ((t (:foreground ,monochwome-tone-1))))
   `(nav-face-file ((t (:foreground ,monochwome-tone-0))))
   `(nav-face-hfile ((t (:foreground ,monochwome-tone-1-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6)))
	  (t
	   (:underline ,monochwome-tone-6))))
   `(merlin-compilation-error-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-1)))
	  (t
	   (:underline ,monochwome-tone-1))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,monochwome-tone-4    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,monochwome-tone-3+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,monochwome-tone-4-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,monochwome-tone-3   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,monochwome-tone-4-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,monochwome-tone-3-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,monochwome-tone-4    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,monochwome-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,monochwome-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,monochwome-bg-2))))
   `(mumamo-background-chunk-submode2 ((t (:background ,monochwome-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,monochwome-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,monochwome-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,monochwome-tone-4+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,monochwome-tone-0))))
   `(neo-root-dir-face ((t (:foreground ,monochwome-tone-4+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,monochwome-tone-4))))
   `(neo-file-link-face ((t (:foreground ,monochwome-tone-0))))
   `(neo-expand-btn-face ((t (:foreground ,monochwome-tone-4))))
   `(neo-vc-default-face ((t (:foreground ,monochwome-tone-0+1))))
   `(neo-vc-user-face ((t (:foreground ,monochwome-tone-1 :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,monochwome-tone-0))))
   `(neo-vc-edited-face ((t (:foreground ,monochwome-tone-7))))
   `(neo-vc-needs-merge-face ((t (:foreground ,monochwome-tone-1+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,monochwome-tone-1 :background ,monochwome-tone-4-5))))
   `(neo-vc-added-face ((t (:foreground ,monochwome-tone-3+1))))
   `(neo-vc-conflict-face ((t (:foreground ,monochwome-tone-1+1))))
   `(neo-vc-missing-face ((t (:foreground ,monochwome-tone-1+1))))
   `(neo-vc-ignored-face ((t (:foreground ,monochwome-tone-0-1))))
;;;;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(notmuch-crypto-part-header ((t (:foreground ,monochwome-tone-4+1))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,monochwome-bg :background ,monochwome-tone-1))))
   `(notmuch-crypto-signature-good ((t (:foreground ,monochwome-bg :background ,monochwome-tone-3+1))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,monochwome-bg :background ,monochwome-tone-1))))
   `(notmuch-hello-logo-background ((t (:background ,monochwome-bg+2))))
   `(notmuch-message-summary-face ((t (:background ,monochwome-bg-1))))
   `(notmuch-search-flagged-face ((t (:foreground ,monochwome-tone-4+1))))
   `(notmuch-search-non-matching-authors ((t (:foreground ,monochwome-tone-0-1))))
   `(notmuch-tag-added ((t (:underline ,monochwome-tone-3+1))))
   `(notmuch-tag-deleted ((t (:strike-through ,monochwome-tone-1))))
   `(notmuch-tag-face ((t (:foreground ,monochwome-tone-3+1))))
   `(notmuch-tag-flagged ((t (:foreground ,monochwome-tone-4+1))))
   `(notmuch-tag-unread ((t (:foreground ,monochwome-tone-1))))
   `(notmuch-tree-match-author-face ((t (:foreground ,monochwome-tone-3+1))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,monochwome-tone-3+1))))
;;;;; orderless
   `(orderless-match-face-0 ((t (:foreground ,monochwome-tone-3))))
   `(orderless-match-face-1 ((t (:foreground ,monochwome-tone-7))))
   `(orderless-match-face-2 ((t (:foreground ,monochwome-tone-4))))
   `(orderless-match-face-3 ((t (:foreground ,monochwome-tone-6))))
;;;;; org-mode
   `(org-agenda-date-today
	 ((t (:foreground ,monochwome-tone-0+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
	 ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,monochwome-tone-0 :weight bold))))
   `(org-block ((t (:background ,monochwome-bg :extend t :family "Iosevka" :weight light))))
   ;; `(org-block ((,class (:foreground ,mono-fg :background ,mono-bg :inherit fixed-pitch))))
   ;; `(org-block-begin-line ((t (:foreground ,mono-orange-5 :background ,monochwome-bg :inherit fixed-pitch))))
   ;; `(org-block-background ((t (:background ,monochwome-bg :inherit fixed-pitch))))
   ;; `(org-block-end-line ((t (:foreground ,mono-orange-5 :background ,monochwome-bg :inherit fixed-pitch))))
   `(org-code ((t (:foreground ,monochwome-tone-0-2 :inherit default))))
   `(org-checkbox ((t (:background ,monochwome-bg+2 :foreground ,monochwome-tone-0+1
				   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,monochwome-tone-4 :underline t))))
   `(org-drawer ((t (:foreground ,monochwome-tone-0-4))))
   `(org-deadline-announce ((t (:foreground ,monochwome-tone-1-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,monochwome-tone-3+3))))
   `(org-formula ((t (:foreground ,monochwome-tone-2-2))))
   `(org-headline-done ((t (:foreground ,monochwome-tone-3+3))))
   `(org-hide ((t (:foreground ,monochwome-bg))))
   `(org-level-1 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0 :weight bold
				   ,@(when monochwome-scale-org-headlines
				   (list :height monochwome-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0-2
				   ,@(when monochwome-scale-org-headlines
				   (list :height monochwome-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+4
				   ,@(when monochwome-scale-org-headlines
				   (list :height monochwome-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-0+2
				   ,@(when monochwome-scale-org-headlines
				   (list :height monochwome-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-5))))
   `(org-level-6 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-3+2))))
   `(org-level-7 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-1+2))))
   `(org-level-8 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-7))))
   `(org-link ((t (:foreground ,monochwome-tone-2-2 :underline t))))
   `(org-quote ((t (:background ,monochwome-bg+1 :extend t))))
   `(org-scheduled ((t (:foreground ,monochwome-tone-3+4))))
   `(org-scheduled-previously ((t (:foreground ,monochwome-tone-1))))
   `(org-scheduled-today ((t (:foreground ,monochwome-tone-4+1))))
   `(org-sexp-date ((t (:foreground ,monochwome-tone-4+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,monochwome-tone-3+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org--grid ((t (:foreground ,monochwome-tone-6))))
   `(org-todo ((t (:weight bold :foreground ,monochwome-tone-1 :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-verbatim ((t (:foreground ,monochwome-tone-0-1 :box (:line-width 1 :foreground ,monochwome-tone-0-3)))))
   `(org-warning ((t (:weight bold :foreground ,monochwome-tone-1 :weight bold :underline nil))))
   `(org-column ((t (:background ,monochwome-bg-2))))
   `(org-column-title ((t (:background ,monochwome-bg-2 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg-2))))
   `(org-mode-line-clock-overrun ((t (:foreground ,monochwome-bg :background ,monochwome-tone-1-1))))
   `(org-ellipsis ((t (:foreground ,monochwome-tone-2-1 :underline t))))
   `(org-footnote ((t (:foreground ,monochwome-tone-5 :underline t))))
   `(org-document-title ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-4
					  :weight bold
					  ,@(when monochwome-scale-org-headlines
					  (list :height monochwome-height-plus-4))))))
   `(org-document-info ((t (:foreground ,monochwome-tone-4))))
   `(org-document-info-keyword ((t (:foreground ,monochwome-tone-0+3))))
   `(org-habit-ready-face ((t :background ,monochwome-tone-3)))
   `(org-habit-alert-face ((t :background ,monochwome-tone-2-1 :foreground ,monochwome-bg)))
   `(org-habit-clear-face ((t :background ,monochwome-tone-4-3)))
   `(org-habit-overdue-face ((t :background ,monochwome-tone-1-3)))
   `(org-habit-clear-future-face ((t :background ,monochwome-tone-4-4)))
   `(org-habit-ready-future-face ((t :background ,monochwome-tone-3-2)))
   `(org-habit-alert-future-face ((t :background ,monochwome-tone-2-2 :foreground ,monochwome-bg)))
   `(org-habit-overdue-future-face ((t :background ,monochwome-tone-1-4)))
;;;;; org-modern
   `(org-modern-done ((t (:inherit org-modern-label :background ,monochwome-tone-0-5 :foreground ,monochwome-bg-3))))
   `(org-modern-time-active ((t :weight bold :inherit org-modern-label :background ,monochwome-bg-2 :foreground ,monochwome-tone-0-4 :distant-foreground ,monochwome-tone-0-3 :width condensed :weight light)))
   `(org-modern-date-active ((t (:inherit org-modern-time-active))))
   `(org-modern-time-inactive ((t (:inherit org-modern-label :background ,monochwome-bg-2 :foreground ,monochwome-tone-0-3 :distant-foreground ,monochwome-bg :width condensed))))
   `(org-modern-date-inactive ((t (:inherit org-modern-time-inactive))))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-6
				 ,@(when monochwome-scale-outline-headlines
				 (list :height monochwome-height-plus-4))))))
   `(outline-2 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-3+4
				 ,@(when monochwome-scale-outline-headlines
				 (list :height monochwome-height-plus-3))))))
   `(outline-3 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-4-1
				 ,@(when monochwome-scale-outline-headlines
				 (list :height monochwome-height-plus-2))))))
   `(outline-4 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-2-2
				 ,@(when monochwome-scale-outline-headlines
				 (list :height monochwome-height-plus-1))))))
   `(outline-5 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-5))))
   `(outline-6 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-3+2))))
   `(outline-7 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-1-4))))
   `(outline-8 ((t (:inherit ,monochwome-variable-pitch :foreground ,monochwome-tone-4-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,monochwome-tone-7))))
   `(cperl-array-face ((t (:foreground ,monochwome-tone-2, :background ,monochwome-bg))))
   `(cperl-hash-face ((t (:foreground ,monochwome-tone-2-1, :background ,monochwome-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,monochwome-tone-0-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,monochwome-tone-2-2))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,monochwome-bg-1 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,monochwome-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,monochwome-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,monochwome-bg+3 :inherit mode-line-inactive))))
;;;;; prootint-0eneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6))))
   `(proof-error-face ((t (:foreground ,monochwome-tone-0 :background ,monochwome-tone-1-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-2-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6))))
   `(proof-locked-face ((t (:background ,monochwome-tone-4-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6))))
   `(proof-queue-face ((t (:background ,monochwome-tone-1-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,monochwome-tone-1-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,monochwome-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,monochwome-bg))))
   `(proof-warning-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-2-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,monochwome-tone-0-4))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,monochwome-tone-0+3))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,monochwome-tone-0-5))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,monochwome-tone-0+3))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,monochwome-tone-0-3))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,monochwome-tone-0-5))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,monochwome-tone-0+3))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,monochwome-tone-0-4))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,monochwome-tone-0-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,monochwome-tone-0+4))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,monochwome-tone-0+3))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,monochwome-tone-0-5))))
   `(rainbow-delimiters-mismatched-face ((t (:background ,monochwome-tone-0+3 :foreground ,monochwome-tone-0-5))))
   `(rainbow-delimiters-unmatched-face ((t (:background ,monochwome-tone-0+3 :foreground ,monochwome-tone-0-5))))
   `(rainbow-delimiters-pick-face-function ((t (:background ,monochwome-tone-0+3 :foreground ,monochwome-tone-0-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,monochwome-tone-4))))
   `(rcirc-other-nick ((t (:foreground ,monochwome-tone-6))))
   `(rcirc-bright-nick ((t (:foreground ,monochwome-tone-4+1))))
   `(rcirc-dim-nick ((t (:foreground ,monochwome-tone-4-2))))
   `(rcirc-server ((t (:foreground ,monochwome-tone-3))))
   `(rcirc-server-prefix ((t (:foreground ,monochwome-tone-3+1))))
   `(rcirc-timestamp ((t (:foreground ,monochwome-tone-3+2))))
   `(rcirc-nick-in-message ((t (:foreground ,monochwome-tone-2))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,monochwome-tone-2 :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-7))))
   `(reb-match-1 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-4))))
   `(reb-match-2 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6))))
   `(reb-match-3 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-1))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,monochwome-tone-3))))
   `(realgud-overlay-arrow2 ((t (:foreground ,monochwome-tone-2))))
   `(realgud-overlay-arrow3 ((t (:foreground ,monochwome-tone-6))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,monochwome-tone-1 :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,monochwome-tone-2))))
   `(realgud-backtrace-number ((t (:foreground ,monochwome-tone-2, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,monochwome-tone-4-4 :weight bold))))
;;;;; rmail
   `(rmail-highlight ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(rmail-header-name ((t (:foreground ,monochwome-tone-4))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,monochwome-tone-3))))
   `(rpm-spec-doc-face ((t (:foreground ,monochwome-tone-3))))
   `(rpm-spec-ghost-face ((t (:foreground ,monochwome-tone-1))))
   `(rpm-spec-macro-face ((t (:foreground ,monochwome-tone-2))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,monochwome-tone-1))))
   `(rpm-spec-package-face ((t (:foreground ,monochwome-tone-1))))
   `(rpm-spec-section-face ((t (:foreground ,monochwome-tone-2))))
   `(rpm-spec-tag-face ((t (:foreground ,monochwome-tone-4))))
   `(rpm-spec-var-face ((t (:foreground ,monochwome-tone-1))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,monochwome-tone-6))))
   `(rst-level-2-face ((t (:foreground ,monochwome-tone-3+1))))
   `(rst-level-3-face ((t (:foreground ,monochwome-tone-4-1))))
   `(rst-level-4-face ((t (:foreground ,monochwome-tone-2-2))))
   `(rst-level-5-face ((t (:foreground ,monochwome-tone-5))))
   `(rst-level-6-face ((t (:foreground ,monochwome-tone-3-2))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,monochwome-tone-2 :weight bold :underline t))))
   `(selectrum-primary-highlight ((t (:background ,monochwome-tone-3-2))))
   `(selectrum-secondary-highlight ((t (:background ,monochwome-tone-3))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,monochwome-tone-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,monochwome-tone-1+1 :background ,monochwome-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,monochwome-bg-0 :background ,monochwome-tone-0+6 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Monochwome for sml
   `(sml/global ((,class (:foreground ,monochwome-tone-0 :weight bold))))
   `(sml/modes ((,class (:foreground ,monochwome-tone-2 :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,monochwome-tone-0-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,monochwome-tone-2 :weight bold))))
   `(sml/line-number ((,class (:foreground ,monochwome-tone-4 :weight bold))))
   `(sml/col-number ((,class (:foreground ,monochwome-tone-4+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,monochwome-tone-4-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,monochwome-tone-6))))
   `(sml/git ((,class (:foreground ,monochwome-tone-3+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,monochwome-tone-6 :weight bold))))
   `(sml/read-only ((,class (:foreground ,monochwome-tone-1-2))))
   `(sml/outside-modified ((,class (:foreground ,monochwome-tone-6))))
   `(sml/modified ((,class (:foreground ,monochwome-tone-1))))
   `(sml/vc-edited ((,class (:foreground ,monochwome-tone-3+2))))
   `(sml/charging ((,class (:foreground ,monochwome-tone-3+4))))
   `(sml/discharging ((,class (:foreground ,monochwome-tone-1+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,monochwome-tone-1+1 :background ,monochwome-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,monochwome-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,monochwome-tone-1))))
   `(slime-repl-inputed-output-face ((t (:foreground ,monochwome-tone-3))))
   `(slime-error-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-1)))
	  (t
	   (:underline ,monochwome-tone-1))))
   `(slime-warning-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6)))
	  (t
	   (:underline ,monochwome-tone-6))))
   `(slime-style-warning-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-2)))
	  (t
	   (:underline ,monochwome-tone-2))))
   `(slime-note-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-3)))
	  (t
	   (:underline ,monochwome-tone-3))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; SLY
   `(sly-mrepl-output-face ((t (:foreground ,monochwome-tone-1))))
   `(sly-error-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-1)))
	  (t
	   (:underline ,monochwome-tone-1))))
   `(sly-warning-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-6)))
	  (t
	   (:underline ,monochwome-tone-6))))
   `(sly-style-warning-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-2)))
	  (t
	   (:underline ,monochwome-tone-2))))
   `(sly-note-face
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,monochwome-tone-3)))
	  (t
	   (:underline ,monochwome-tone-3))))
   `(sly-stickers-placed-face ((t (:foreground ,monochwome-tone-0 :background ,monochwome-bg+3))))
;;;;; solaire
   `(solaire-default-face ((t (:inherit default :background ,monochwome-bg-2))))
   `(solaire-minibuffer-face ((t (:inherit default :background ,monochwome-bg-3))))
   `(solaire-hl-line-face ((t (:inherit hl-line))))
   `(solaire-org-hide-face ((t (:inherit org-hide :background ,monochwome-bg-1))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,monochwome-tone-3+2))))
   `(speedbar-directory-face ((t (:foreground ,monochwome-tone-5))))
   `(speedbar-file-face ((t (:foreground ,monochwome-tone-0))))
   `(speedbar-highlight-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-3+2))))
   `(speedbar-selected-face ((t (:foreground ,monochwome-tone-1))))
   `(speedbar-separator-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-4-1))))
   `(speedbar-tag-face ((t (:foreground ,monochwome-tone-2))))
;;;;; swiper
   `(swiper-line-face ((t (:underline t))))
;;;;; sx
   `(sx-custom-button
	 ((t (:background ,monochwome-tone-0 :foreground ,monochwome-bg-2
			  :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
	 ((t (:foreground ,monochwome-tone-3+3
			  :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
	 ((t (:foreground ,monochwome-tone-3+3
			  :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
	 ((t (:box (:color ,monochwome-bg-2 :line-width 3 :style released-button)
		   :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,monochwome-tone-0
					:background ,monochwome-bg))))
   `(tabbar-selected ((t (:foreground ,monochwome-tone-0
					  :background ,monochwome-bg
					  :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,monochwome-tone-0
					:background ,monochwome-bg+1
					:box (:line-width -1 :style released-button)))))
;;;;; tab-bar
   `(tab-bar ((t (:background ,monochwome-bg-3))))
   `(tab-bar-tab ((t (:foreground ,monochwome-bg
				  :background ,monochwome-tone-0
				  :weight bold))))
   `(tab-bar-tab-inactive ((t (:foreground ,monochwome-bg
					   :background ,monochwome-tone-0-5))))
;;;;; tab-line
   `(tab-line ((t (:inherit tab-bar))))
   `(tab-line-tab ((t (:inherit tab-bar-tab))))
   `(tab-line-tab-inactive ((t (:inherit tab-bar-tab-inactive))))
   `(tab-line-tab-current ((t (:foreground ,monochwome-bg
					   :background ,monochwome-tone-0))))
;;;;; term
   `(term-color-black ((t (:foreground ,monochwome-bg
					   :background ,monochwome-bg-2))))
   `(term-color-red ((t (:foreground ,monochwome-tone-1-2
					 :background ,monochwome-tone-1-4))))
   `(term-color-green ((t (:foreground ,monochwome-tone-3
					   :background ,monochwome-tone-3+2))))
   `(term-color-yellow ((t (:foreground ,monochwome-tone-6
					:background ,monochwome-tone-2))))
   `(term-color-blue ((t (:foreground ,monochwome-tone-4-1
					  :background ,monochwome-tone-4-4))))
   `(term-color-magenta ((t (:foreground ,monochwome-tone-7
					 :background ,monochwome-tone-1))))
   `(term-color-cyan ((t (:foreground ,monochwome-tone-5
					  :background ,monochwome-tone-4))))
   `(term-color-white ((t (:foreground ,monochwome-tone-0
					   :background ,monochwome-tone-0-1))))
   '(term-default-tint-0-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,monochwome-tone-0+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,monochwome-tone-1-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,monochwome-tone-0))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,monochwome-tone-2))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,monochwome-tone-5))))
;;;;; vertico
   `(vertico-current ((t (:foreground ,monochwome-tone-2 :weight bold :underline t))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-3 :weight bold))))
   `(vr/group-1 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-6 :weight bold))))
   `(vr/group-2 ((t (:foreground ,monochwome-bg :background ,monochwome-tone-4 :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,monochwome-tone-2-2 :background ,monochwome-bg-2 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,monochwome-tone-1 :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,monochwome-bg-1))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,monochwome-tone-6 ))))
   `(web-mode-css-prop-face ((t (:foreground ,monochwome-tone-6))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,monochwome-tone-3+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,monochwome-tone-4))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,monochwome-tone-4))))
   `(web-mode-html-attr-name-face ((t (:foreground ,monochwome-tone-6))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,monochwome-tone-5))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,monochwome-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,monochwome-tone-1))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,monochwome-bg+1 :foreground ,monochwome-bg+1))))
   `(whitespace-hspace ((t (:background ,monochwome-bg+1 :foreground ,monochwome-bg+1))))
   `(whitespace-tab ((t (:background ,monochwome-bg-3))))
   `(whitespace-newline ((t (:foreground ,monochwome-bg-3))))
   `(whitespace-trailing ((t (:background ,monochwome-bg-3))))
   `(whitespace-line ((t (:background ,monochwome-bg-3))))
   `(whitespace-space-before-tab ((t (:background ,monochwome-bg-3 :foreground ,monochwome-tone-6))))
   `(whitespace-indentation ((t (:background ,monochwome-bg-3 :foreground ,monochwome-tone-6))))
   `(whitespace-empty ((t (:background ,monochwome-bg-3))))
   `(whitespace-space-after-tab ((t (:background ,monochwome-bg-3 :foreground ,monochwome-tone-1))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,monochwome-tone-1-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,monochwome-tone-1-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,monochwome-tone-6))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,monochwome-tone-4))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,monochwome-tone-0))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,monochwome-tone-4))))
   `(wl-highlight-message-citation-header ((t (:foreground ,monochwome-tone-1-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,monochwome-tone-1))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,monochwome-tone-3+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,monochwome-tone-4))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,monochwome-tone-4+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,monochwome-tone-3))))
   `(wl-highlight-message-headers-face ((t (:foreground ,monochwome-tone-1+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,monochwome-tone-3+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,monochwome-tone-3+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,monochwome-tone-3+2))))
   `(wl-highlight-message-signature ((t (:foreground ,monochwome-tone-3))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,monochwome-tone-0))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,monochwome-tone-4))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,monochwome-tone-0
							 :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,monochwome-tone-4))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,monochwome-tone-0))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,monochwome-tone-2))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,monochwome-tone-7))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,monochwome-tone-0))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,monochwome-tone-3+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,monochwome-tone-2 :weight bold))))
   `(cscope-function-face ((t (:foreground ,monochwome-tone-5 :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,monochwome-tone-1 :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,monochwome-bg :background ,monochwome-tone-4+1))))
   `(cscope-separator-face ((t (:foreground ,monochwome-tone-1 :weight bold
						:underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,monochwome-bg-2))))
   `(yascroll:thumb-fringe ((t (:background ,monochwome-bg-2 :foreground ,monochwome-bg-2))))
   ))

;;; Theme Variables
(monochwome-with-color-variables
  (custom-theme-set-variables
   'monochwome
;;;;; ansi-color
   `(ansi-color-names-vector [,monochwome-bg ,monochwome-tone-1 ,monochwome-tone-3 ,monochwome-tone-2
					  ,monochwome-tone-4 ,monochwome-tone-7 ,monochwome-tone-5 ,monochwome-tone-0])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,monochwome-bg+1)
   `(company-quickhelp-color-foreground ,monochwome-tone-0)
;;;;; fill-column-indicator
   `(fci-rule-color ,monochwome-bg-1)
;;;;; nrepl-client
   `(nrepl-message-colors
	 '(,monochwome-tone-1 ,monochwome-tone-6 ,monochwome-tone-2 ,monochwome-tone-3 ,monochwome-tone-3+4
	   ,monochwome-tone-5 ,monochwome-tone-4+1 ,monochwome-tone-7))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,monochwome-tone-0 . ,monochwome-bg-1))
;;;;; vc-annotate
   `(vc-annotate-color-map
	 '(( 20. . ,monochwome-tone-1-1)
	   ( 40. . ,monochwome-tone-1)
	   ( 60. . ,monochwome-tone-6)
	   ( 80. . ,monochwome-tone-2-2)
	   (100. . ,monochwome-tone-2-1)
	   (120. . ,monochwome-tone-2)
	   (140. . ,monochwome-tone-3-2)
	   (160. . ,monochwome-tone-3)
	   (180. . ,monochwome-tone-3+1)
	   (200. . ,monochwome-tone-3+2)
	   (220. . ,monochwome-tone-3+3)
	   (240. . ,monochwome-tone-3+4)
	   (260. . ,monochwome-tone-5)
	   (280. . ,monochwome-tone-4-2)
	   (300. . ,monochwome-tone-4-1)
	   (320. . ,monochwome-tone-4)
	   (340. . ,monochwome-tone-4+1)
	   (360. . ,monochwome-tone-7)))
   `(vc-annotate-very-old-color ,monochwome-tone-7)
   `(vc-annotate-background ,monochwome-bg-2)
   ))

  ;;; custom theme variables
  (custom-theme-set-variables
   'monochwome

   ;; tab bar
   `(tab-bar-close-button-show nil)
   `(tab-bar-new-button-show nil)

   ;; fringe
   `(fringe-mode '(12 . 12))

   ;; diff-hl
   `(diff-hl-margin-mode t)

   ;; doom
   `(doom-modeline-icon t)
   `(doom-modeline-height 25)
   `(doom-modeline-major-mode-color-icon nil)
   `(doom-modeline-bar-width 1)
   )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
		   (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun monochwome-high-contrast-comment-mode()
  (interactive)
  (if monochwome-high-contrast-comments
	  (progn
	(set-face-foreground font-lock-comment-face
				 (eval (alist-get (intern "monochwome-tone-0-4") monochwome-default-colors-alist)))
	(setq monochwome-high-contrast-comments nil))
	(progn
	  ;; FWR we have to intern the string into a key to get the alist value at key. Isn't that fun.
	  ;; Actually, no, this don't work.
	  (set-face-foreground font-lock-comment-face
			   (eval (alist-get (intern "monochwome-tone-0-5") monochwome-default-colors-alist)))
	  (setq monochwome-high-contrast-comments t))))

(provide-theme 'monochwome)

;;; monochwome-theme.el ends here.
