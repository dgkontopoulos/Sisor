(load "~/.sbclrc")

(require 'asdf)
(asdf:operate 'asdf:load-op :cl-gtk2-gtk)

(defun run ()
  (gtk:within-main-loop
   (let ((window (make-instance  'gtk:gtk-window
				 :title "Sisor"
				 :type :toplevel
				 :window-position :mouse
				 :resize t))
	 (vbox (make-instance 'gtk:v-box))
	 (hbox1 (make-instance 'gtk:h-box))
	 (new_button (make-instance 'gtk:button
				    :label "gtk-new"
				    :use-stock t))
	 (open_button (make-instance 'gtk:button
				     :label "gtk-open"
				     :use-stock t))
	 (edit_button (make-instance 'gtk:button
				     :label "gtk-edit"
				     :use-stock t))
	 (delete_button (make-instance 'gtk:button
				       :label "gtk-delete"
				       :use-stock t))
	 (about_button (make-instance 'gtk:button
				      :label "gtk-about"
				      :use-stock t))
	 (exit_button (make-instance 'gtk:button
				     :label "gtk-quit"
				     :use-stock t))
	 (space_name (make-instance 'gtk:label
				    :label "<span size='large'><b>Untitled space</b></span>"
				    :selectable t
				    :use-markup t))
	 (space_photo (make-instance 'gtk:image
				     :file "./images/default_space.png")))

     (gobject:g-signal-connect about_button "clicked"
			       (lambda (b) (about window)))
     (gtk:container-add hbox1 new_button)
     (gtk:container-add hbox1 open_button)
     (gtk:container-add hbox1 edit_button)
     (gtk:container-add hbox1 delete_button)
     (gtk:container-add hbox1 about_button)

     (gobject:g-signal-connect exit_button "clicked"
			       (lambda (b) (gtk:object-destroy window)))
     (gtk:container-add hbox1 exit_button)

     (gtk:container-add vbox hbox1)
     (gtk:container-add vbox space_name)
     (gtk:container-add vbox space_photo)
     (gtk:container-add window vbox)
     (gtk:widget-show window :all :t))))

(defun about (window)
  (gtk:within-main-loop
   (defvar default_text "<span size='small'>
<b>Sisor</b> was initially developed for the <a href='http://lispinsummerprojects.org/'>2013 Lisp in Summer
Projects programming contest</a>. The code is written in
<a href='http://common-lisp.net/index.html'>Common Lisp</a>/<a href='http://www.cliki.net/cl-gtk2'>CL-GTK2</a>.

The application's <a href='http://en.wikipedia.org/wiki/Recursive_acronym#Computer-related_examples'>recursive acronym</a> and logo have been
inspired from <b><i><a href='http://en.wikipedia.org/wiki/Sisor'>Sisor rabdophorus</a></i></b>.</span>
")
   (let ((about_window (make-instance  'gtk:gtk-window
				       :title "About Sisor"
				       :type :toplevel
				       :border-width 10
				       :window-position :mouse
				       :destroy-with-parent t
				       :transient-for window))
	 (vbox (make-instance 'gtk:v-box))
	 (logo (make-instance 'gtk:image
			      :file "./images/sisor_card.png"))
	 (top_info (make-instance 'gtk:label
				  :label "<b>S</b>isor; <b>I</b>ntelligent <b>S</b>pace <b>Or</b>ganization, v. 1.0

Homepage
(C) 2013 <a href='mailto:dgkontopoulos@member.fsf.org?Subject=Sisor'>Dimitrios - Georgios Kontopoulos</a>
"
				  :use-markup t
				  :justify :center))
	 (second_info (make-instance 'gtk:label
				     :label default_text
				     :use-markup t))
	 (hbox (make-instance 'gtk:h-box))
	 (license_button (make-instance 'gtk:toggle-button
					:label  "License"))
	 (thanks_button (make-instance 'gtk:toggle-button
				       :label  "Thanks"))
	 (quit_button (make-instance 'gtk:button
				     :label "gtk-quit"
				     :use-stock t)))

     (gtk:container-add vbox logo)
     (gtk:container-add vbox top_info)
     (gtk:container-add vbox (make-instance 'gtk:h-separator))
     (gtk:container-add vbox second_info)
     (gtk:container-add vbox (make-instance 'gtk:h-separator))

     (defvar license_counter 1)
     (defvar thanks_counter 1)

     (gobject:g-signal-connect license_button "toggled"
			       (lambda (b)
				 (if (eql license_counter 1)
				     (progn (setf (gtk:label-label second_info) "
<span size='small'><b><u>License:</u></b>
<i>This program is free software; you can redistribute it and/or
modify it under the terms of the <a href='http://www.gnu.org/licenses/gpl.html'>GNU General Public License,
as published by the Free Software Foundation; either version
3 of the License, or (at your option) any later version</a>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.</i></span>
")
					    (gtk:widget-show about_window :all :t)
					    (setf license_counter 0)
					    (setf thanks_counter 1))
				     (progn (setf (gtk:label-label second_info) default_text)
					    (setf license_counter 1)
					    (setf thanks_counter 1)))))
     (gtk:container-add hbox license_button)

     (gobject:g-signal-connect thanks_button "toggled"
			       (lambda (b)
				 (if (eql thanks_counter 1)
				     (progn (setf (gtk:label-label second_info) "
<span size='small'><b><u>Thanks to:</u></b>
	➜<b><a href='http://reimenaashelyee.tumblr.com/'>Reimena Ashel Yee</a></b> for designing Sisor's logo.</span>
")
					    (gtk:widget-show about_window :all :t)
					    (setf thanks_counter 0)
					    (setf license_counter 1))
				     (progn (setf (gtk:label-label second_info) default_text)
					    (setf thanks_counter 1)
					    (setf license_counter 1)))))
     (gtk:container-add hbox thanks_button)

     (gobject:g-signal-connect quit_button "clicked"
			       (lambda (b) (gtk:object-destroy about_window)))
     (gtk:container-add hbox quit_button)

     (gtk:container-add vbox hbox)
     (gtk:container-add about_window vbox)
     (gtk:widget-show about_window :all :t))))

(run)
