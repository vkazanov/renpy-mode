# Copyright (C) 2017 Eliza Velasquez
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Imported from https://github.com/elizagamedev/renpy-mode

# USAGE: run the renpy cli tool with RENPY_EDIT_PY environment variable set to
# this file.

import renpy
import subprocess
import os.path


class Editor(renpy.editor.Editor):

    def begin(self, new_window=False, **kwargs):
        if renpy.windows:
            self.arguments = ["emacsclientw", "-n"]
        else:
            self.arguments = ["emacsclient", "-n"]

    def open(self, filename, line=None, **kwargs):
        # Handle special cases for generated files like lint.txt, errors.txt,
        # and traceback.txt; specifically treat them as read-only special-mode
        # buffers
        basename = os.path.basename(filename)
        filename = renpy.exports.fsencode(filename)
        if basename in {"lint.txt", "errors.txt", "traceback.txt"}:
            # special-mode buffer
            buffer_name = "*renpy {}*".format(os.path.splitext(basename)[0])
            filename = filename.replace(os.sep, '/')
            self.arguments.extend(
                ('-e',
                 '(let ((inhibit-read-only t))' +
                 '(get-buffer-create "{}")'.format(buffer_name) +
                 '(switch-to-buffer-other-window "{}")'.format(buffer_name) +
                 '(insert-file-contents "{}" nil nil nil t)'.format(filename) +
                 '(special-mode))'))
        else:
            # normal file
            if line:
                self.arguments.append("+%d" % line)
            self.arguments.append(filename)

    def end(self, **kwargs):
        subprocess.Popen(self.arguments)
