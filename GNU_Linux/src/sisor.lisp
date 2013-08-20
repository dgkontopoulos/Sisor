;(load "~/.sbclrc")

;(require 'asdf)
;(asdf:operate 'asdf:load-op :cl-gtk2-gtk)
;(asdf:operate 'asdf:load-op :cl-ppcre)

(defun run ()
  (gtk:within-main-loop
   (let ((window (make-instance  'gtk:gtk-window
				 :title "Sisor"
				 :type :toplevel
				 :window-position :mouse
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
				    :text "Untitled space"
				    :has-frame nil
				    :xalign 0.5))
	 (space_photo (make-instance 'gtk:image
				     :file "./images/default_space.png"))
	 (select_hbox (make-instance 'gtk:h-box))
	 (select_image (make-instance 'gtk:button
				      :label "Select a photo for this space"))
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
	 (image_select_button (make-instance 'gtk:file-chooser-button
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
			       (lambda (b) (about window)))
     (gtk:container-add hbox1 new_button)
     (gtk:container-add hbox1 open_button)
     (gtk:container-add hbox1 edit_button)

     (gobject:g-signal-connect delete_button "clicked"
			       (lambda (b) (delete-reask)))
     (gtk:container-add hbox1 delete_button)

     (gtk:container-add hbox1 about_button)

     (gobject:g-signal-connect exit_button "clicked"
			       (lambda (b)
				 (gtk:object-destroy window)
				 (exit :abort t)))
     (gtk:container-add hbox1 exit_button)

     (gtk:container-add vbox1 hbox1)
     (gtk:container-add vbox2 space_name)
     (gtk:container-add vbox2 space_photo)

     (gobject:g-signal-connect select_image "clicked"
			       (lambda (b)
				 (setf (gtk:button-label select_image) "Select a different photo")
				 (setf (gtk:widget-sensitive delete_image) t)))
     (gtk:container-add select_hbox select_image)

     (gobject:g-signal-connect delete_image "clicked"
			       (lambda (b)
				 (setf (gtk:button-label select_image) "Select a photo for this space")
				 (setf (gtk:widget-sensitive delete_image) nil)
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
     (gtk:container-add new_item_hbox image_select_button)
     (gtk:container-add add_item_vbox new_item_hbox)

     (gtk:container-add naming_hbox new_item_name)
     (gtk:container-add naming_hbox item_entry)
     (gtk:container-add add_item_vbox naming_hbox)

     (gobject:g-signal-connect add_button "clicked"
			       (lambda (b)
				 (if (or (not (eq (cl-ppcre:scan "^\\s*$" (gtk:entry-text item_entry)) nil))
					 (eq (cl-ppcre:scan "png|jpg|jpeg|gif$" (gtk:file-chooser-filename image_select_button)) nil))
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
			       (lambda (b)
				 (exit :abort t)))
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
					    (setf thanks_counter 1)
					    (setf (gtk:toggle-button-active thanks_button) nil))
				     (progn (setf (gtk:label-label second_info) default_text)
					    (setf license_counter 1)
					    (setf (gtk:toggle-button-active thanks_button) nil)
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
					    (setf license_counter 1)
					    (setf (gtk:toggle-button-active license_button) nil))
				     (progn (setf (gtk:label-label second_info) default_text)
					    (setf thanks_counter 1)
					    (setf (gtk:toggle-button-active license_button) nil)
					    (setf license_counter 1)))))
     (gtk:container-add hbox thanks_button)

     (gobject:g-signal-connect quit_button "clicked"
			       (lambda (b) (gtk:object-destroy about_window)))
     (gtk:container-add hbox quit_button)

     (gtk:container-add vbox hbox)
     (gtk:container-add about_window vbox)
     (gtk:widget-show about_window :all :t))))

(defun delete-reask ()
  (gtk:within-main-loop
   (let ((window (make-instance  'gtk:gtk-window
				 :type :popup
				 :window-position :mouse
				 :border-width 5))
	 (vbox (make-instance 'gtk:v-box))
	 (hbox1 (make-instance 'gtk:h-box))
	 (warning (make-instance 'gtk:image
				 :stock "gtk-dialog-warning"))
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
			       (lambda (b) (gtk:object-destroy window)))
     (gtk:container-add hbox2 no_button)

     (gobject:g-signal-connect yes_button "clicked"
			       (lambda (b) (gtk:object-destroy window)))
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
				   :stock "gtk-dialog-error"))
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
			       (lambda (b) (gtk:object-destroy window)))
     (gtk:container-add vbox close_button)
     (gtk:container-add window vbox)
     (gtk:widget-show window :all :t))))

(run)
