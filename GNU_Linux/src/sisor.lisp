
; DEPENDENCIES: cl-gtk2-gtk, cl-ppcre, cl-fad


(defparameter *entered-main-interface* 0)

(defun about (window)
  (gtk:within-main-loop
   (let ((default_text "<span size='small'>
<b>Sisor</b> was initially developed for the <a href='http://lispinsummerprojects.org/'>2013 Lisp in Summer
Projects programming contest</a>. The code is written in
<a href='http://common-lisp.net/index.html'>Common Lisp</a>/<a href='http://www.cliki.net/cl-gtk2'>CL-GTK2</a>.

The application's <a href='http://en.wikipedia.org/wiki/Recursive_acronym#Computer-related_examples'>recursive acronym</a> and logo have been
inspired from <b><i><a href='http://en.wikipedia.org/wiki/Sisor'>Sisor rabdophorus</a></i></b>.</span>
"))
     (let ((about_window (make-instance  'gtk:gtk-window
					 :title "About Sisor"
					 :icon (gtk:image-pixbuf
						(make-instance 'gtk:image
							       :file "./images/sisor_fish.png"))
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
	   (license_counter 1)
	   (thanks_button (make-instance 'gtk:toggle-button
					 :label  "Thanks"))
	   (thanks_counter 1)
	   (quit_button (make-instance 'gtk:button
				       :label "gtk-quit"
				       :use-stock t)))

       (gtk:container-add vbox logo)
       (gtk:container-add vbox top_info)
       (gtk:container-add vbox (make-instance 'gtk:h-separator))
       (gtk:container-add vbox second_info)
       (gtk:container-add vbox (make-instance 'gtk:h-separator))

       (gobject:g-signal-connect license_button "toggled"
				 #'(lambda (b)
				     (declare (ignorable b))
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
						(setf thanks_counter 1)
						(setf (gtk:toggle-button-active thanks_button) nil))
				       (progn (setf (gtk:label-label second_info) default_text)
					      (setf license_counter 1)
					      (setf (gtk:toggle-button-active thanks_button) nil)
					      (setf thanks_counter 1)))))
       (gtk:container-add hbox license_button)

       (gobject:g-signal-connect thanks_button "toggled"
				 #'(lambda (b)
				     (declare (ignorable b))
				     (if (eql thanks_counter 1)
					 (progn (setf (gtk:label-label second_info) "
<span size='small'><b><u>Thanks to:</u></b>
	➜<b><a href='http://reimenaashelyee.tumblr.com/'>Reimena Ashel Yee</a></b> for designing Sisor's logo.</span>
")
						(gtk:widget-show about_window :all :t)
						(setf thanks_counter 0)
						(setf license_counter 1)
						(setf (gtk:toggle-button-active license_button) nil))
				       (progn (setf (gtk:label-label second_info) default_text)
					      (setf thanks_counter 1)
					      (setf (gtk:toggle-button-active license_button) nil)
					      (setf license_counter 1)))))
       (gtk:container-add hbox thanks_button)

       (gobject:g-signal-connect quit_button "clicked"
				 #'(lambda (b)
				     (declare (ignorable b))
				     (gtk:object-destroy about_window)))
       (gtk:container-add hbox quit_button)

       (gtk:container-add vbox hbox)
       (gtk:container-add about_window vbox)
       (gtk:widget-show about_window :all :t)))))

(defun directory-check (dir)
  (setf dir (cl-ppcre:regex-replace "/?$" dir "/"))
  (and
   (cl-fad:directory-exists-p dir)
   (eql (list-length (directory (concatenate 'string dir "*"))) 0)))

(defun new-project (window)
  (let ((dir_dialog
	 (make-instance 'gtk:file-chooser-dialog
			:title "Select a directory for the new Project"
			:action :select-folder
			:local-only t
			:do-overwrite-confirmation t)))

    (gtk:dialog-add-button dir_dialog "gtk-cancel" :cancel)
    (gtk:dialog-add-button dir_dialog "gtk-ok" :ok)

    (gobject:connect-signal dir_dialog "response"
			    (lambda (dir_dialog response)
			      (cond ((eq response -6)
				     (gtk:object-destroy dir_dialog))
				    ((eq response -5)
				     (progn
				       (setf *entered-main-interface* 1)
				       (main-interface)
				       (gtk:object-destroy dir_dialog)
				       (gtk:object-destroy window))))))

    (gtk:widget-show dir_dialog)))


(defun starting-popup ()
  (gtk:within-main-loop
   (let ((window (make-instance 'gtk:gtk-window
				:type :toplevel
				:icon (gtk:image-pixbuf
				       (make-instance 'gtk:image
						      :file "./images/sisor_fish.png"))
				:title "Sisor"
				:window-position :center
				:decorated nil
				:allow-grow nil
				:allow-shrink nil
				:border-width 20))
	 (hbox_main (make-instance 'gtk:h-box :spacing 10))

	 (fish_logo (make-instance 'gtk:image
				   :file "./images/sisor_fish.png"))

	 (vbox_welcome (make-instance 'gtk:v-box :spacing 30))
	 (name_logo (make-instance 'gtk:image
				   :file "./images/sisor_name.png"))

	 (buttons_vbox_1 (make-instance 'gtk:v-box :spacing 50))

	 (new_event (make-instance 'gtk:event-box))
	 (new_hbox (make-instance 'gtk:h-box :spacing 5))
	 (new_image (make-instance 'gtk:image
				   :stock "gtk-new"
				   :icon-size 3))
	 (new_label (make-instance 'gtk:label
				   :label "<b>New Project</b>"
				   :use-markup t))

	 (about_event (make-instance 'gtk:event-box))
	 (about_hbox (make-instance 'gtk:h-box :spacing 5))
	 (about_image (make-instance 'gtk:image
				     :stock "gtk-about"
				     :icon-size 3))
	 (about_label (make-instance 'gtk:label
				     :label "<b>About Sisor</b>"
				     :use-markup t))

	 (buttons_vbox_2 (make-instance 'gtk:v-box :spacing 50))

	 (open_event (make-instance 'gtk:event-box))
	 (open_hbox (make-instance 'gtk:h-box :spacing 5))
	 (open_image (make-instance 'gtk:image
				    :stock "gtk-open"
				    :icon-size 3))
	 (open_label (make-instance 'gtk:label
				    :label "<b>Open Project</b>"
				    :use-markup t))

	 (quit_event (make-instance 'gtk:event-box))
	 (quit_hbox (make-instance 'gtk:h-box :spacing 5))
	 (quit_image (make-instance 'gtk:image
				    :stock "gtk-quit"
				    :icon-size 3))
	 (quit_label (make-instance 'gtk:label
				    :label "<b>Quit Sisor</b>"
				    :use-markup t))

	 (buttons_hbox (make-instance 'gtk:h-box :spacing 20)))

     (gtk:container-add hbox_main fish_logo)

     (gtk:container-add vbox_welcome name_logo)

     (gtk:container-add new_hbox new_image)
     (gtk:container-add new_hbox new_label)
     (gtk:container-add new_event new_hbox)

     (gtk:widget-modify-bg new_event 0 (gdk:color-parse "#FFFFFF"))

     (gobject:g-signal-connect new_event "button_press_event"
			       #'(lambda (a b)
				   (declare (ignorable a b))
				   (new-project window)))
     (gtk:container-add buttons_vbox_1 new_event)

     (gtk:container-add about_hbox about_image)
     (gtk:container-add about_hbox about_label)
     (gtk:container-add about_event about_hbox)

     (gtk:widget-modify-bg about_event 0 (gdk:color-parse "#FFFFFF"))

     (gobject:g-signal-connect about_event "button_press_event"
			       #'(lambda (a b)
				   (declare (ignorable a b))
				   (about window)))
     (gtk:container-add buttons_vbox_1 about_event)

     (gtk:container-add buttons_hbox buttons_vbox_1)

     (gtk:container-add open_hbox open_image)
     (gtk:container-add open_hbox open_label)
     (gtk:container-add open_event open_hbox)

     (gtk:widget-modify-bg open_event 0 (gdk:color-parse "#FFFFFF"))
     (gtk:container-add buttons_vbox_2 open_event)

     (gtk:container-add quit_hbox quit_image)
     (gtk:container-add quit_hbox quit_label)
     (gtk:container-add quit_event quit_hbox)

     (gtk:widget-modify-bg quit_event 0 (gdk:color-parse "#FFFFFF"))

     (gobject:g-signal-connect quit_event "button_press_event"
			       #'(lambda (a b)
				   (declare (ignorable a b))
				   (gtk:object-destroy window)
				   (exit :abort t)))
     (gtk:container-add buttons_vbox_2 quit_event)

     (gtk:container-add buttons_hbox buttons_vbox_2)

     (gtk:container-add vbox_welcome buttons_hbox)

     (gtk:container-add hbox_main vbox_welcome)

     (gtk:container-add window hbox_main)

     (gobject:g-signal-connect window "destroy"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (if (eq *entered-main-interface* 0)
				       (exit :abort t))))
     (gtk:widget-show window :all :t))))

(starting-popup)


(defun check-if-defined (item)
  (cond
   ((string-equal item "space_name")
    (if (not (boundp '*space_name*)) "Untitled space"))
   ((string-equal item "space_photo")
    (if (not (boundp '*space_photo*)) "./images/default_space.png"))
   (t "")))

(defun delete-reask ()
  (gtk:within-main-loop
   (let ((window (make-instance  'gtk:gtk-window
				 :type :popup
				 :window-position :mouse
				 :border-width 5))
	 (vbox (make-instance 'gtk:v-box))
	 (hbox1 (make-instance 'gtk:h-box))
	 (warning (make-instance 'gtk:image
				 :stock "gtk-dialog-warning"
				 :icon-size 3))
	 (label1 (make-instance 'gtk:label
				:label
				"This action will <b>delete the current project
and any data linked to it</b>."
				:use-markup t))
	 (label2 (make-instance 'gtk:label
				:label "Is that OK?"))
	 (hbox2 (make-instance 'gtk:h-box))
	 (no_button (make-instance 'gtk:button
				   :label "gtk-no"
				   :use-stock t))
	 (yes_button (make-instance 'gtk:button
				    :label "gtk-yes"
				    :use-stock t)))

     (gtk:container-add hbox1 warning)
     (gtk:container-add hbox1 label1)
     (gtk:container-add vbox hbox1)
     (gtk:container-add vbox label2)
     (gtk:container-add vbox (make-instance 'gtk:h-separator))

     (gobject:g-signal-connect no_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (gtk:object-destroy window)))
     (gtk:container-add hbox2 no_button)

     (gobject:g-signal-connect yes_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (gtk:object-destroy window)))
     (gtk:container-add hbox2 yes_button)

     (gtk:container-add vbox hbox2)
     (gtk:container-add window vbox)
     (gtk:widget-show window :all :t))))

(defun empty-item-name ()
  (gtk:within-main-loop
   (let ((window (make-instance  'gtk:gtk-window
				 :type :popup
				 :window-position :mouse
				 :border-width 5))
	 (vbox (make-instance 'gtk:v-box))
	 (hbox1 (make-instance 'gtk:h-box))
	 (error_pic (make-instance 'gtk:image
				   :stock "gtk-dialog-error"
				   :image-size 3))
	 (error_message (make-instance 'gtk:label
				       :label
				       "<b>ERROR!</b>
Either a valid image file (png, jpg, jpeg or gif) or
an item name was not provided!
"
				       :use-markup t))
	 (close_button (make-instance 'gtk:button
				      :label "gtk-close"
				      :use-stock t)))
     (gtk:container-add hbox1 error_pic)
     (gtk:container-add hbox1 error_message)
     (gtk:container-add vbox hbox1)

     (gobject:g-signal-connect close_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (gtk:object-destroy window)))
     (gtk:container-add vbox close_button)
     (gtk:container-add window vbox)
     (gtk:widget-show window :all :t))))

(defun main-interface ()
  (gtk:within-main-loop
   (let ((window (make-instance  'gtk:gtk-window
				 :title "Sisor"
				 :icon (gtk:image-pixbuf
					(make-instance 'gtk:image
						       :file "./images/sisor_fish.png"))
				 :type :toplevel
				 :window-position :center
				 :resize t))
	 (vbox1 (make-instance 'gtk:v-box))
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
	 (hbox2 (make-instance 'gtk:h-box
			       :spacing 5))
	 (vbox2 (make-instance 'gtk:v-box))
	 (space_name (make-instance 'gtk:entry
				    :text (check-if-defined "space_name")
				    :has-frame nil
				    :xalign 0.5))
	 (space_photo (make-instance 'gtk:image
				     :file (check-if-defined "space_photo")))
	 (select_hbox (make-instance 'gtk:h-box))
	 (select_label (make-instance 'gtk:label
				      :label "Select a photo for this space:"))
	 (image_filter (make-instance 'gtk:file-filter
				      :name "Image files (*.png, *.jpg, *.jpeg, *.tif, *.tiff, *.bmp)"))
	 (select_button (make-instance 'gtk:file-chooser-button
				       :select-multiple nil
				       :width-chars 20
				       :title "Select a photo for this space"))
	 (delete_image (make-instance 'gtk:button
				      :label "Remove the photo"
				      :sensitive nil))
	 (managing_hbox (make-instance 'gtk:h-box
				       :spacing 20))
	 (inventory_vbox (make-instance 'gtk:v-box))
	 (inventory (make-instance 'gtk:label
				   :label "<u><b>Inventory</b></u>"
				   :use-markup t))
	 (inventory_list (make-instance 'gtk:combo-box))
	 (item_hbox (make-instance 'gtk:h-box))
	 (item_label (make-instance 'gtk:label
				    :label "<i>Item name</i>"
				    :use-markup t))
	 (item_image (make-instance 'gtk:image
				    :file "./images/default_item.png"))
	 (item_actions_hbox (make-instance 'gtk:h-box))
	 (remove_item (make-instance 'gtk:button
				     :label "Remove this item"
				     :sensitive nil))
	 (move_item (make-instance 'gtk:button
				   :label "Move to another space"
				   :sensitive nil))
	 (add_item_vbox (make-instance 'gtk:v-box
				       :spacing 23))
	 (add_item (make-instance 'gtk:label
				  :label "<u><b>Add new item</b></u>"
				  :use-markup t))
	 (new_item_hbox (make-instance 'gtk:h-box
				       :spacing 10))
	 (image_file_label (make-instance 'gtk:label
					  :label "Image file:"))
	 (item_select_button (make-instance 'gtk:file-chooser-button
					    :width-request 150))
	 (naming_hbox (make-instance 'gtk:h-box
				     :spacing 10))
	 (new_item_name (make-instance 'gtk:label
				       :label "Item name:"))
	 (item_entry (make-instance 'gtk:entry))
	 (add_button (make-instance 'gtk:button
				    :label "Add to inventory"))
	 (side_vbox (make-instance 'gtk:v-box))
	 (other_spaces (make-instance 'gtk:label
				      :label "<b>Other spaces in this project</b>"
				      :use-markup t))
	 (spaces_list (make-instance 'gtk:tree-view)))

     (gobject:g-signal-connect about_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (about window)))
     (gtk:container-add hbox1 new_button)
     (gtk:container-add hbox1 open_button)
     (gtk:container-add hbox1 edit_button)

     (gobject:g-signal-connect delete_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (delete-reask)))
     (gtk:container-add hbox1 delete_button)

     (gtk:container-add hbox1 about_button)

     (gobject:g-signal-connect exit_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (gtk:object-destroy window)
				   (exit :abort t)))
     (gtk:container-add hbox1 exit_button)

     (gtk:container-add vbox1 hbox1)
     (gtk:container-add vbox2 space_name)
     (gtk:container-add vbox2 space_photo)

     (gtk:container-add select_hbox select_label)
     (gtk:file-filter-add-pixbuf-formats image_filter)

     (gtk:file-chooser-add-filter select_button image_filter)

     (gobject:g-signal-connect select_button "file-set"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (setf (gtk:widget-sensitive delete_image) t)
				   (let ((temp_pixbuf
					  (gdk:pixbuf-new-from-file (format nil "~{~A~}"
									    (gtk:file-chooser-filenames select_button)))))

				     (setf (gtk:image-pixbuf space_photo) temp_pixbuf))))
     (gtk:container-add select_hbox select_button)

     (gobject:g-signal-connect delete_image "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (setf (gtk:widget-sensitive delete_image) nil)
				   (setf (gtk:file-chooser-current-name select_button) "huh")
				   (setf (gtk:image-file space_photo) "./images/default_space.png")))
     (gtk:container-add select_hbox delete_image)
     (gtk:container-add vbox2 select_hbox)

     (gtk:container-add vbox2 (make-instance 'gtk:h-separator))

     (gtk:container-add inventory_vbox inventory)
     (gtk:container-add inventory_vbox inventory_list)

     (gtk:container-add item_hbox item_label)
     (gtk:container-add item_hbox item_image)
     (gtk:container-add inventory_vbox item_hbox)

     (gtk:container-add item_actions_hbox remove_item)
     (gtk:container-add item_actions_hbox move_item)
     (gtk:container-add inventory_vbox item_actions_hbox)

     (gtk:container-add managing_hbox inventory_vbox)
     (gtk:container-add managing_hbox (make-instance 'gtk:v-separator))

     (gtk:container-add add_item_vbox add_item)

     (gtk:container-add new_item_hbox image_file_label)

     (gtk:file-chooser-add-filter item_select_button image_filter)
     (gtk:container-add new_item_hbox item_select_button)
     (gtk:container-add add_item_vbox new_item_hbox)

     (gtk:container-add naming_hbox new_item_name)
     (gtk:container-add naming_hbox item_entry)
     (gtk:container-add add_item_vbox naming_hbox)

     (gobject:g-signal-connect add_button "clicked"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (if (or (not (eq (cl-ppcre:scan "^\\s*$" (gtk:entry-text item_entry)) nil))
					   (eq (cl-ppcre:scan "png|jpg|jpeg|gif$" (gtk:file-chooser-filename item_select_button)) nil))
				       (empty-item-name))))

     (gtk:container-add add_item_vbox add_button)

     (gtk:container-add managing_hbox add_item_vbox)

     (gtk:container-add vbox2 managing_hbox)

     (gtk:container-add hbox2 vbox2)
     (gtk:container-add hbox2 (make-instance 'gtk:v-separator))

     (gtk:box-pack-start side_vbox other_spaces :expand nil)
     (gtk:container-add side_vbox spaces_list)
     (gtk:container-add hbox2 side_vbox)

     (gtk:container-add vbox1 hbox2)

     (gtk:container-add window vbox1)

     (gobject:g-signal-connect window "destroy"
			       #'(lambda (b)
				   (declare (ignorable b))
				   (exit :abort t)))
     (gtk:widget-show window :all :t))))

