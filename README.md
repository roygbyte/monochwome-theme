# Monochwome

Regress your Emacs to one-color world.

![Screenshot of Monochwome](monochwome.png "Monochwome (orange)")
![Screenshot of Monochwome](monochwome-green.png "Monochwome (green)")

Currently just a proof-of-concept. Works reasonably well as a theme for ELisp hacking and org-mode writing.

# Features

- Customize the hue (`M-x customize-variable [RET] monochwome-seed-hue`)
- Adjust contrast ratio between the lightest and darkest *foreground* colors
- Adjust contrast ratio between the lightest and darkest *background* colors

# Installation

Clone the repo. Then add the theme's path to Emac's `custom-theme-load-path` variable.

``` elisp
(add-to-list 'custom-theme-load-path "/path/to/monochwome-theme/") ;; update

```

Finally, load the theme using `M-x load-theme [RET] monochwome`.

# Acknowledgements

The base code for this theme was inherited from zenburn-theme.el.
