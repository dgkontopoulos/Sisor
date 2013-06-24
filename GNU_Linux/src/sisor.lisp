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
								
     (gtk:container-add hbox1 new_button)
     (gtk:container-add hbox1 open_button)
     (gtk:container-add hbox1 edit_button)
     (gtk:container-add hbox1 delete_button)
     (gtk:container-add hbox1 about_button)
     (gtk:container-add hbox1 exit_button)
     (gtk:container-add vbox hbox1)
     (gtk:container-add vbox space_name)
     (gtk:container-add vbox space_photo)
     (gtk:container-add window vbox)
     (gtk:widget-show window :all :t))))

(run)
