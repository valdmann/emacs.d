---
name: profile-startup
description: Profile Emacs startup
---

To profile Emacs startup with the built-in profiler:

```bash
emacs --script scripts/profile-startup.el
```

To print the `use-package'` report (loading times per package):

```bash
emacs --script scripts/use-package-report.el
```

All reports are written to stdout.

Note: scripts should be run from this file's directory.
