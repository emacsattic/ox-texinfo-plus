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

2. Optionally share a section's node with some or all of its child
   sections.  By default every section on every level gets its own
   node, and `ox-texinfo` provides no mechanism for changing that.
   To place a section in the same node as its parent section, do
   this:

   ```
   **** Log Performance
   :PROPERTIES:
   :NONODE: t
   :END:
   ```
  
3. Optionally modify the Org file before exporting it.  This is
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

4. Fully respect the local value of `indent-tabs-mode` from the Org
   file when editing source blocks and exporting.  This affects all
   source blocks and all exporters.

   I recommend you add this at the end of Org files to avoid
   strange indentation, at least with the `texinfo` exporter:

   ```
   # Local Variables:
   # indent-tabs-mode: nil
   # End:
   ```
