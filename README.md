This package is going to be obsolete soon
=========================================

If you create a new manual and are thinking about using `ox-texinfo+` in
addition to `ox-texinfo`, then you should instead use the development version
of Org.

Org's `main` branch (which will be released as Org-9.6 eventually) contains
an improved implementation of the main feature (1) of this package.  It works
basically the same as described below, but you now have to use a "description
list", which essentially means that the first line of all its items have to
end with ` ::`.  For more information see the *Plain lists in Texinfo export*
section in Org's info manual.

I have also added support for automatic use the `@itemx` command where
appropriate, which is described in the same section.

The other features described below have not made it into the new, merged
implementation.

1. The merged feature.
2. I now recommend using Org macros to add version strings and such.
3. This does not appear to be necessary anymore; if it ever was.
4. I would still like this behavior but think it should be implemented
   by patching `org-cycle`.  I have heard rumors that that command is
   being rewritten, so I will wait until that is done, before teaching
   that function about my preferences.

Extensions for Org's Texinfo exporter
=====================================

This package provides some extensions for Org's `texinfo` exporter
defined in `ox-texinfo`.

1. Create `@deffn` and similar definition items by writing list
   items in Org that look similar to what they will look like in
   Info.  To enable this, add:

   ```
   #+TEXINFO_DEFFN: t
   ```

   to your Org file.  After doing that, you can create definition
   items like so:

   ```
   - Command: magit-section-show

     Show the body of the current section.

   - Function: magit-git-exit-code &rest args
   - Macro: magit-insert-section &rest args
   - Variable: magit-display-buffer-noselect
   - User Option: magit-display-buffer-function
   - Key: q, magit-mode-bury-buffer
   ```

2. Optionally modify the Org file before exporting it.  This is
   implemented using a hook that can be set using the `BIND`
   property:

   ```
   #+BIND: ox-texinfo+-before-export-hook some-function
   #+BIND: ox-texinfo+-before-export-hook another-function
   ```
   
   The function `ox-texinfo+-update-version-strings` is provided
   as an example.  It makes some assumptions that might not be
   appropriate for your manuals, so you might have to define your
   own variant.

3. Fully respect the local value of `indent-tabs-mode` from the Org
   file when editing source blocks and exporting.  This affects all
   source blocks and all exporters.

   I recommend you add this at the end of Org files to avoid
   strange indentation, at least with the `texinfo` exporter:

   ```
   # Local Variables:
   # indent-tabs-mode: nil
   # End:
   ```

4. Optionally dissolve certain headlines tilted "_" when using the
   `texinfo` exporter.  This is useful when you want a headline's
   section to be collapsed when `org-cycle` reaches the CONTENTS
   state, just like the sections of sub-headlines are collapsed in
   that state, while in the export you do not want that sub-heading,
   which would be redundant outside of Org where similar visibility
   folding is not available.

   If the first child of a headline is a sub-headline titled "_",
   then the sub-headline is removed and its section is used as the
   section of the parent headline.

   I recommend that you enable this in individual Org files:

   ```
   # Local Variables:
   # eval: (require 'ox-texinfo+ nil t)
   # org-texinfo+-dissolve-noexport-headlines: t
   # End:
   ```
