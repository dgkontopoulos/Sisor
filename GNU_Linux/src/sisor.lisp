
;;;; LISP DEPENDENCIES: cl-gtk2-gtk, cl-ppcre, cl-fad, sqlite
;;;; OTHER DEPENDENCIES: libgtk2.0-0, imagemagick, sqlite3

(in-package :cl-user)

(asdf:oos 'asdf:load-op 'sisor)

(defpackage :sisor
  (:use :cl :cl-fad :cl-ppcre :gobject :gdk :gtk :sqlite)
  (:import-from :asdf :run-shell-command)
  (:import-from :sb-ext :exit))

(in-package :sisor)

(defparameter *top-dir* "")
(defparameter *current-dir* "")
(defparameter *db* "")
(defparameter *entered-main-interface* 0)
(defparameter *inv-count* 0)
(defparameter *spaces-count* 0)

(defun about (window)
  "Creates the 'About Sisor' window."
  (within-main-loop
   (let ((default_text "<span size='small'>
<b>Sisor</b> was initially developed for the <a href='http://lispinsummerprojects.org/'>2013 Lisp in Summer
Projects programming contest</a>. The code is written in
<a href='http://common-lisp.net/index.html'>Common Lisp</a>/<a href='http://www.cliki.net/cl-gtk2'>CL-GTK2</a>.

The application's <a href='http://en.wikipedia.org/wiki/Recursive_acronym#Computer-related_examples'>recursive acronym</a> and logo have been
inspired from <b><i><a href='http://en.wikipedia.org/wiki/Sisor'>Sisor rabdophorus</a></i></b>.</span>
"))
     (let ((about_window (make-instance 'gtk-window
					:title "About Sisor"
					:icon (image-pixbuf
					       (make-instance 'image
							      :file "./images/sisor_minilogo.png"))
					:type :toplevel
					:border-width 10
					:window-position :mouse
					:resizable nil
					:destroy-with-parent t
					:transient-for window))
	   (vbox (make-instance 'v-box))
	   (logo (make-instance 'image
				:file "./images/sisor_card.png"))
	   (top_info (make-instance 'label
				    :label "<b>S</b>isor; <b>I</b>ntelligent <b>S</b>pace <b>Or</b>ganization, v. 1.0

<a href='http://dgkontopoulos.github.io/Sisor/'>Website</a> -- <a href='https://github.com/dgkontopoulos/Sisor'>Source Code</a>
(C) 2013-14 <a href='mailto:dgkontopoulos@member.fsf.org?Subject=Sisor'>Dimitrios - Georgios Kontopoulos</a>
"
				    :use-markup t
				    :justify :center))
	   (second_info (make-instance 'label
				       :label default_text
				       :use-markup t))
	   (hbox (make-instance 'h-box))
	   (license_button (make-instance 'toggle-button
					  :label  "License"))
	   (license_counter 1)
	   (thanks_button (make-instance 'toggle-button
					 :label  "Thanks"))
	   (thanks_counter 1)
	   (quit_button (make-instance 'button
				       :label "gtk-quit"
				       :use-stock t)))

       (container-add vbox logo)
       (container-add vbox top_info)
       (container-add vbox (make-instance 'h-separator))
       (container-add vbox second_info)
       (container-add vbox (make-instance 'h-separator))

	   ;; License tab		;
       (g-signal-connect license_button "toggled"
			 #'(lambda (b)
			     (declare (ignorable b))
			     (if (eql license_counter 1)
				 (progn (setf (label-label second_info) "
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
					(widget-show about_window :all :t)
					(setf license_counter 0)
					(setf thanks_counter 1)
					(setf (toggle-button-active thanks_button) nil))
			       (progn (setf (label-label second_info) default_text)
				      (setf license_counter 1)
				      (setf (toggle-button-active thanks_button) nil)
				      (setf thanks_counter 1)))))
       (container-add hbox license_button)

	   ;; Thanks tab		;
       (g-signal-connect thanks_button "toggled"
			 #'(lambda (b)
			     (declare (ignorable b))
			     (if (eql thanks_counter 1)
				 (progn (setf (label-label second_info) "
<span size='small'><b><u>Thanks to:</u></b>
	➜<b><a href='http://reimenaashelyee.tumblr.com/'>Reimena Ashel Yee</a></b> for designing Sisor's logo.</span>
")
					(widget-show about_window :all :t)
					(setf thanks_counter 0)
					(setf license_counter 1)
					(setf (toggle-button-active license_button) nil))
			       (progn (setf (label-label second_info) default_text)
				      (setf thanks_counter 1)
				      (setf (toggle-button-active license_button) nil)
				      (setf license_counter 1)))))
       (container-add hbox thanks_button)

       (g-signal-connect quit_button "clicked"
			 #'(lambda (b)
			     (declare (ignorable b))
			     (object-destroy about_window)))
       (container-add hbox quit_button)

       (container-add vbox hbox)
       (container-add about_window vbox)
       (widget-show about_window :all :t)))))

(defun directory-check (status dir)
  "Makes sure that a directory exists, is writable and readable.
Also returns relevant information about it."

  ;; Make sure that the directory ends with '/'. ; ;
  (setf dir (regex-replace "/?$" dir "/"))
  (and
   (directory-exists-p dir)
   (ignore-errors (open
		   (concatenate 'string dir ".foo_and_refooo_foo.sisor")
		   :direction :probe :if-does-not-exist :create))
   (ignore-errors (delete-file (concatenate 'string dir ".foo_and_refooo_foo.sisor")))
   (cond ((string-equal status "new")

	  ;; Check whether the directory is empty (zero contents). ;
	  (eql (list-length (directory (concatenate 'string dir "*.*"))) 0))
	 ((string-equal status "open")

	  ;; Check whether the sisor database is accessible. ;
	  (file-exists-p (concatenate 'string dir "sisor.sqlite3")))
	 ((string-equal status "import") t))))

(defun failure (item)
  "Displays an error message, when something somewhere went wrong."
  (let ((dialog
	 (make-instance 'message-dialog
			:message-type :error
			:buttons :close
			:secondary-use-markup t)))

    (cond

     ;; Error in choosing a directory for a new Project. ;
     ((string-equal item "new-directory")
      (setf (message-dialog-text dialog) "Inappropriate Directory")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> The directory is not empty.

<b>=></b> The directory does not exist.

<b>=></b> Sisor does not have the permissions to create or modify files in that directory."))

     ;; Error in opening a Project from a specific directory. ;
     ((string-equal item "open-directory")
      (setf (message-dialog-text dialog) "Inappropriate Directory")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> The directory does not contain a Sisor Project.

<b>=></b> The directory does not exist.

<b>=></b> Sisor does not have the permissions to create, view or modify files in that directory."))

     ;; Error when renaming the current space.
     ((string-equal item "modify-name")
      (setf (message-dialog-text dialog) "Error when modifying the space's name")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> There is already a space with that name in the current Project.

<b>=></b> The name contains illegal characters. Names can contain word characters and underscores.

<b>=></b> The name was not changed."))

     ;; Error when selecting a photo for the current space.
     ((string-equal item "space_photo")
      (setf (message-dialog-text dialog) "Inappropriate File")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> A valid image file (png, jpg, jpeg or gif) was not selected.

<b>=></b> The file does not exist.

<b>=></b> Sisor does not have the permissions to read that file."))

     ;; Error when adding an item to the inventory.
     ((string-equal item "space_item")
      (setf (message-dialog-text dialog) "Inappropriate Item")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> A valid image file (png, jpg, jpeg or gif) was not selected.

<b>=></b> The file does not exist.

<b>=></b> Sisor does not have the permissions to read that file.

<b>=></b> A name for the item was not provided.

<b>=></b> There is already an item with this name in this space."))

     ;; Error when adding a new space.
     ((string-equal item "add-space")
      (setf (message-dialog-text dialog) "Error when adding a new space")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> There is already a space with that name in the current Project.

<b>=></b> The name contains illegal characters. Names can contain word characters and underscores."))

     ;; Error when importing a compressed Sisor Project.
     ((string-equal item "import")
      (setf (message-dialog-text dialog) "Importing Error")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> A Sisor Project file and/or an output directory were not selected.

<b>=></b> Sisor does not have the permissions to read the file.

<b>=></b> Sisor does not have the permissions to create files in the directory."))

     ;; Error when exporting a Sisor Project.
     ((string-equal item "export")
      (setf (message-dialog-text dialog) "Exporting Error")
      (setf (message-dialog-secondary-text dialog) "
<b>=></b> There is already a file with the same name at the specified destination.

<b>=></b> Sisor does not have the permissions to create the file it that directory.")))

    ;; Add standard text.
    (setf (message-dialog-secondary-text dialog)
	  (concatenate 'string "One of the following situations has occurred:
" (message-dialog-secondary-text dialog) "

Please, resolve the problem and try again."))

    (connect-signal dialog "response"
		    #'(lambda (dialog response)
			(if (eq response -7)
			    (object-destroy dialog))))
    (widget-show dialog)))

(defun create-space (name)
  "Performs the necessary actions to introduce a new space."
  (if (string-equal name "default")
      (progn

	;; Create the directories.
	(ensure-directories-exist
	 (concatenate 'string *top-dir* "Untitled_space/items/"))

	;; Create a new table in the sqlite3 database.
	(execute-non-query *db* "create table Untitled_space
	(item text, location text, previous_locations text, description text)")
	(setf *current-dir* (concatenate 'string *top-dir* "Untitled_space/")))
    (progn

      ;; Create the directories.
      (ensure-directories-exist (concatenate 'string *top-dir* name "/items/"))

      ;; Create a new table in the sqlite3 database.
      (execute-non-query *db*
			 (concatenate 'string "create table '" name
				      "' (item text, location text, previous_locations text, description text)"))
      (setf *current-dir* (concatenate 'string *top-dir* name "/")))))

(defun find-spaces (dirs spaces_list)
  "Returns a list of the available spaces within the Project."
  (if (eql (list-length dirs) 0)
      spaces_list
    (progn
      (setf spaces_list
	    (append spaces_list (last (pathname-directory (car dirs)))))
      (find-spaces (cdr dirs) spaces_list))))

(defun open-project ()
  "Opens the Project, selects the first available space
and checks whether a main space photo exists."
  (let ((available_projects
	 (find-spaces (directory
		       (concatenate 'string *top-dir* "*/")) '())))

    (setf *current-dir* (concatenate 'string *top-dir*
				     (car available_projects) "/"))
    (defparameter *space_name* (car available_projects))

    ;; Check whether this space already has a main photo.
    (if (file-exists-p (concatenate 'string *current-dir* "space_photo"))
	(defparameter *space_photo*
	  (concatenate 'string *current-dir* "space_photo"))
      (makunbound '*space_photo*))))

(defun prepare-main-interface (window dialog status)
  "Prepares the main window for opening a specific Project."
  (cond ((eq *entered-main-interface* 0) (setf *entered-main-interface* 1))
	((eq *entered-main-interface* 1)
	 (setf *entered-main-interface* 0)
	 (disconnect *db*)))

  ;; Make sure that the directory ends with '/'.
  (setf *top-dir* (regex-replace "/?$"
				 (file-chooser-filename dialog) "/"))
  (setf *space_name* "Untitled_space")

  ;; Connect to the sqlite3 database.
  (setf *db* (connect (concatenate 'string *top-dir* "sisor.sqlite3")))

  ;; Set the inventory and spaces counts to zero.
  (setf *inv-count* 0)
  (setf *spaces-count* 0)
  (makunbound '*current-item*)
  (makunbound '*current-item-name*)
  (object-destroy dialog)
  (object-destroy window)

  (if (eq *entered-main-interface* 0) (setf *entered-main-interface* 1))

  (cond ((string-equal status "new") (create-space "default"))
	((string-equal status "open") (open-project)))
  (main-interface))


(defun get-project (window status)
  "Creates a file chooser dialog for opening an existing space
or creating a new one."
  (let ((dir_dialog
	 (make-instance 'file-chooser-dialog
			:action :select-folder
			:local-only t)))

    (cond ((string-equal status "new")
	   (setf (gtk-window-title dir_dialog)
		 "Select a directory for the new Project"))
	  ((string-equal status "open")
	   (setf (gtk-window-title dir_dialog)
		 "Select a Sisor Project's top directory")))

    (dialog-add-button dir_dialog "gtk-cancel" :cancel)
    (dialog-add-button dir_dialog "gtk-ok" :ok)

    (connect-signal dir_dialog "response"
		    #'(lambda (dir_dialog response)

			;; Actions when Cancel is pressed.
			(cond ((eq response -6)
			       (object-destroy dir_dialog))

			      ;; Actions when OK is pressed.
			      ((eq response -5)
			       (cond ((string-equal status "new")
				      (if (directory-check "new"
							   (file-chooser-filename dir_dialog))
					  (prepare-main-interface window dir_dialog "new")
					(failure "new-directory")))
				     ((string-equal status "open")
				      (if (directory-check "open"
							   (file-chooser-filename dir_dialog))
					  (prepare-main-interface window dir_dialog "open")
					(failure "open-directory"))))))))

    (widget-show dir_dialog)))

(defun readable-p (file)
  "Checks whether a particular file exists and is readable."
  (and
   (file-exists-p file)
   (ignore-errors (open file))))

(defun import-success (import_dialog dir)
  "Creates a message that informs the user upon successful
Project importing."
  (let ((dialog (make-instance 'message-dialog
			       :message-type :info
			       :buttons :ok
			       :text "Success!"
			       :secondary-text
			       (concatenate 'string
					    "The Project's top directory is now located inside "
					    dir " ."))))

    (connect-signal dialog "response"
		    #'(lambda (dialog response)
			(object-destroy dialog)
			(object-destroy import_dialog)))

    (widget-show dialog)))

(defun import-project (window)
  "Creates the window that allows the user to import an
existing compressed Project."
  (let ((dialog (make-instance 'dialog
			       :window-position :center
			       :title "Import a Sisor Project"
			       :resizable nil
			       :border-width 5))
	(vbox (make-instance 'v-box))
	(file_hbox (make-instance 'h-box :spacing 10))
	(input_chooser (make-instance 'file-chooser-button
				      :select-multiple nil
				      :width-chars 14))
	(file_filter (make-instance 'file-filter
				    :name "Compressed tar archives (*.tar.gz)"))
	(dir_hbox (make-instance 'h-box :spacing 10))
	(output_chooser (make-instance 'file-chooser-button
				       :title "Select a directory for the Sisor Project"
				       :action :select-folder))
	(input_file "")
	(output_dir ""))

    (container-add file_hbox
		   (make-instance 'label
				  :label "<b>Sisor Project file:</b>"
				  :use-markup t))

    ;; Make sure that only *.tar.gz files are listed.
    (file-filter-add-pattern file_filter "*.tar.gz")
    (file-chooser-add-filter input_chooser file_filter)
    (container-add file_hbox input_chooser)
    (container-add vbox file_hbox)

    (container-add dir_hbox
		   (make-instance 'label
				  :label "<b>Output directory:</b>"
				  :use-markup t))
    (container-add dir_hbox output_chooser)
    (container-add vbox dir_hbox)

    (dialog-add-button dialog "gtk-cancel" :cancel)
    (dialog-add-button dialog "gtk-ok" :ok)

    (connect-signal dialog "response"
		    #'(lambda (photo_dialog response)

			;; Action when Cancel is pressed.
			(cond ((eq response -6)
			       (object-destroy dialog))

			      ;; Actions when OK is pressed.
			      ((eq response -5)

			       (setf input_file (file-chooser-filename input_chooser))
			       (setf output_dir (file-chooser-filename output_chooser))

			       (if (and
				    (not (eq input_file nil))
				    (readable-p input_file)
				    (not (eq output_dir nil))
				    (directory-check "import" output_dir))
				   (progn

				     ;; Extract a compressed archive with tar.
				     (run-shell-command (concatenate 'string
								     "tar xzf '" input_file "' -C '"
								     output_dir "'"))
				     (import-success dialog output_dir))
				 (failure "import"))))))

    (container-add (dialog-content-area dialog) vbox)
    (widget-show dialog)))

(defun delete-reask (status window &key name hbox name_button space_photo
			    button1 button2 item_image item_description table
			    remove_button vbox inventory_list item_hbox)
  "Creates a dialog that asks the user again upon their request
of project/space deletion."

  ;; Make sure that the user really wants to delete the current project.
  (cond ((string-equal status "project")
	 (let ((dialog (make-instance 'message-dialog
				      :message-type :warning
				      :buttons :yes-no
				      :text "This action will delete the current project
and any data linked to it."
				      :secondary-text "Is that OK?")))

	   (connect-signal dialog "response"
			   #'(lambda (dialog response)

			       ;; Action when No is pressed.
			       (cond ((eq response -9)
				      (object-destroy dialog))

				     ;; Actions when Yes is pressed.
				     ((eq response -8)
				      (start-from-scratch)
				      (starting-popup)
				      (object-destroy dialog)
				      (object-destroy window)))))

	   (widget-show dialog)))

	;; When the user tries to delete the last space, ask them
	;; if they want to delete the whole project.
	((and (eq *spaces-count* 2) (string-equal status "space"))
	 (let ((dialog (make-instance 'message-dialog
				      :message-type :warning
				      :buttons :yes-no
				      :text (concatenate 'string "This is the last space for this Project.")
				      :secondary-text "Do you want to delete the Project?")))

	   (connect-signal dialog "response"
			   #'(lambda (dialog response)

			       ;; Action when No is pressed.
			       (cond ((eq response -9)
				      (object-destroy dialog))

				     ;; Actions when Yes is pressed.
				     ((eq response -8)
				      (start-from-scratch)
				      (starting-popup)
				      (object-destroy dialog)
				      (object-destroy window)))))

	   (widget-show dialog)))

	;; Make sure that the user really wants to delete the selected space.
	((string-equal status "space")
	 (let ((dialog (make-instance 'message-dialog
				      :message-type :warning
				      :buttons :yes-no
				      :text (concatenate 'string "This action will delete '" name
							 "' and any data linked to it.")
				      :secondary-text "Is that OK?")))

	   (connect-signal dialog "response"
			   #'(lambda (dialog response)

			       ;; Action when No is pressed.
			       (cond ((eq response -9)
				      (object-destroy dialog))

				     ;; Actions when Yes is pressed.
				     ((eq response -8)

				      ;; Delete the directory and files.
				      (delete-directory-and-files
				       (concatenate 'string *top-dir* name "/"))

				      ;; Drop the sqlite3 table.
				      (execute-non-query *db*
							 (concatenate 'string "drop table '" name "'"))

				      ;; Switch to the first available space.
				      (setf *space_name*
					    (car (find-spaces
						  (directory (concatenate 'string *top-dir* "*/"))
						  '())))
				      (setf *current-dir*
					    (concatenate 'string *top-dir* *space_name* "/"))

				      (switch-space name_button space_photo button1
						    button2 item_image item_description table
						    remove_button vbox inventory_list item_hbox)

				      (decf *spaces-count*)
				      (object-destroy hbox)
				      (object-destroy dialog)))))

	   (widget-show dialog)))))

(defun create-menubar (window)
  "Creates the application's menu."
  (let ((menubar (make-instance 'menu-bar))

	;; Project menu.
	(menu_item_project (make-instance 'menu-item :label "Project"))
	(menu_project (make-instance 'menu))

	;; Project menu items.
	(menu_project_new (make-instance 'menu-item :label "New Project"))
	(menu_project_open (make-instance 'menu-item :label "Open Project"))
	(menu_project_delete (make-instance 'menu-item :label "Delete Project"))
	(menu_project_import (make-instance 'menu-item :label "Import Project"))
	(menu_project_export (make-instance 'menu-item :label "Export Project"))
	(menu_project_close (make-instance 'menu-item :label "Close Project"))
	(menu_project_exit (make-instance 'menu-item :label "Exit"))

	;; Help menu.
	(menu_item_help (make-instance 'menu-item :label "Help"))
	(menu_help (make-instance 'menu))

	;; Help menu items.
	(menu_help_cl (make-instance 'menu-item :label "About Common Lisp"))
	(menu_help_clgtk2 (make-instance 'menu-item :label "About CL-GTK2"))
	(menu_help_sisor (make-instance 'menu-item :label "About Sisor")))

    (if (eq *entered-main-interface* 0)
	(progn
	  (setf (widget-sensitive menu_project_delete) nil)
	  (setf (widget-sensitive menu_project_export) nil)
	  (setf (widget-sensitive menu_project_close) nil)))

    ;; New Project.
    (g-signal-connect menu_project_new "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (get-project window "new")))
    (menu-shell-append menu_project menu_project_new)

    ;; Open Project.
    (g-signal-connect menu_project_open "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (get-project window "open")))
    (menu-shell-append menu_project menu_project_open)

    ;; Separator.
    (menu-shell-append menu_project
		       (make-instance 'separator-menu-item))

    ;; Delete Project.
    (g-signal-connect menu_project_delete "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (delete-reask "project" window)))
    (menu-shell-append menu_project menu_project_delete)

    ;; Separator.
    (menu-shell-append menu_project
		       (make-instance 'separator-menu-item))

    ;; Import Project.
    (g-signal-connect menu_project_import "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (import-project window)))
    (menu-shell-append menu_project menu_project_import)

    ;; Export Project.
    (g-signal-connect menu_project_export "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (export-project)))
    (menu-shell-append menu_project menu_project_export)

    ;; Separator.
    (menu-shell-append menu_project
		       (make-instance 'separator-menu-item))

    ;; Close Project.
    (g-signal-connect menu_project_close "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (start-from-scratch "nodelete")
			  (starting-popup)
			  (object-destroy window)))
    (menu-shell-append menu_project menu_project_close)

    ;; Exit Project.
    (g-signal-connect menu_project_exit "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (if (eq *entered-main-interface* 1)
			      (disconnect *db*))
			  (exit :abort t)))
    (menu-shell-append menu_project menu_project_exit)

    (setf (menu-item-submenu menu_item_project) menu_project)

    (menu-shell-append menubar menu_item_project)

    ;; About Common Lisp.
    (g-signal-connect menu_help_cl "activate"
		      #'(lambda (a)
			  (declare (ignorable a))

			  ;; Open the site using the system's default browser.
			  (run-shell-command
			   "xdg-open https://en.wikipedia.org/wiki/Common_Lisp&")))
    (menu-shell-append menu_help menu_help_cl)

    ;; About cl-gtk2.
    (g-signal-connect menu_help_clgtk2 "activate"
		      #'(lambda (a)
			  (declare (ignorable a))

			  ;; Open the site using the system's default browser.
			  (run-shell-command
			   "xdg-open http://www.cliki.net/cl-gtk2&")))
    (menu-shell-append menu_help menu_help_clgtk2)

    ;; Separator.
    (menu-shell-append menu_help
		       (make-instance 'separator-menu-item))

    ;; About Sisor.
    (g-signal-connect menu_help_sisor "activate"
		      #'(lambda (a)
			  (declare (ignorable a))
			  (about window)))
    (menu-shell-append menu_help menu_help_sisor)

    (setf (menu-item-submenu menu_item_help) menu_help)

    (menu-shell-append menubar menu_item_help)

    menubar))

(defun starting-popup ()
  "Creates an introductory popup window for the user to
select among opening a Project, creating a new one or
importing one."
  (within-main-loop
   (let ((window (make-instance 'gtk-window
				:type :toplevel
				:icon (image-pixbuf
				       (make-instance 'image
						      :file "./images/sisor_minilogo.png"))
				:title "Sisor"
				:window-position :center
				:decorated nil
				:allow-grow nil
				:allow-shrink nil
				:border-width 20))
	 (vbox_main (make-instance 'v-box :spacing 15))
	 (hbox_main (make-instance 'h-box :spacing 10))

	 (fish_logo (make-instance 'image
				   :file "./images/sisor_fish.png"))

	 (vbox_welcome (make-instance 'v-box :spacing 30))
	 (name_logo (make-instance 'image
				   :file "./images/sisor_name.png"))

	 (buttons_vbox_1 (make-instance 'v-box :spacing 30))

	 (new_event (make-instance 'event-box))
	 (new_hbox (make-instance 'h-box :spacing 5))
	 (new_image (make-instance 'image
				   :stock "gtk-new"
				   :icon-size 3))
	 (new_label (make-instance 'label
				   :label "<b>New Project</b>"
				   :use-markup t))

	 (about_event (make-instance 'event-box))
	 (about_hbox (make-instance 'h-box :spacing 5))
	 (about_image (make-instance 'image
				     :stock "gtk-about"
				     :icon-size 3))
	 (about_label (make-instance 'label
				     :label "<b>About Sisor</b>"
				     :use-markup t))

	 (buttons_vbox_2 (make-instance 'v-box :spacing 30))

	 (open_event (make-instance 'event-box))
	 (open_hbox (make-instance 'h-box :spacing 5))
	 (open_image (make-instance 'image
				    :stock "gtk-open"
				    :icon-size 3))
	 (open_label (make-instance 'label
				    :label "<b>Open Project</b>"
				    :use-markup t))

	 (quit_event (make-instance 'event-box))
	 (quit_hbox (make-instance 'h-box :spacing 5))
	 (quit_image (make-instance 'image
				    :stock "gtk-quit"
				    :icon-size 3))
	 (quit_label (make-instance 'label
				    :label "<b>Quit Sisor</b>"
				    :use-markup t))

	 (buttons_hbox (make-instance 'h-box :spacing 20)))

     (container-add vbox_main (create-menubar window))

     ;; Fish logo.
     (container-add hbox_main fish_logo)

     ;; Name.
     (container-add vbox_welcome name_logo)

     ;; New Project, packed in a white event box.
     (container-add new_hbox new_image)
     (container-add new_hbox new_label)
     (container-add new_event new_hbox)

     (widget-modify-bg new_event 0 (color-parse "#FFFFFF"))

     (g-signal-connect new_event "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (get-project window "new")))
     (container-add buttons_vbox_1 new_event)

     ;; About Sisor, packed in a white event box.
     (container-add about_hbox about_image)
     (container-add about_hbox about_label)
     (container-add about_event about_hbox)

     (widget-modify-bg about_event 0 (color-parse "#FFFFFF"))

     (g-signal-connect about_event "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (about window)))
     (container-add buttons_vbox_1 about_event)

     (container-add buttons_hbox buttons_vbox_1)

     ;; Open Project, packed in a white event box.
     (container-add open_hbox open_image)
     (container-add open_hbox open_label)
     (container-add open_event open_hbox)

     (widget-modify-bg open_event 0 (color-parse "#FFFFFF"))

     (g-signal-connect open_event "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (get-project window "open")))
     (container-add buttons_vbox_2 open_event)

     ;; Quit Sisor, packed in a white event box.
     (container-add quit_hbox quit_image)
     (container-add quit_hbox quit_label)
     (container-add quit_event quit_hbox)

     (widget-modify-bg quit_event 0 (color-parse "#FFFFFF"))

     (g-signal-connect quit_event "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (object-destroy window)
			   (exit :abort t)))
     (container-add buttons_vbox_2 quit_event)

     (container-add buttons_hbox buttons_vbox_2)

     (container-add vbox_welcome buttons_hbox)

     (container-add hbox_main vbox_welcome)

     (container-add vbox_main hbox_main)
     (container-add window vbox_main)

     (g-signal-connect window "destroy"
		       #'(lambda (b)
			   (declare (ignorable b))
			   (if (eq *entered-main-interface* 0)
			       (exit :abort t))))
     (widget-show window :all :t))))

(starting-popup)


(defun check-defined (item)
  "Checks whether the space has a custom name or main photo."
  (cond
   ((string-equal item "space_name")
    (if (not (boundp '*space_name*)) "Untitled space" *space_name*))
   ((string-equal item "space_photo")
    (if (not (boundp '*space_photo*)) "./images/default_space.png" *space_photo*))
   (t "")))

(defun start-from-scratch (&optional status)
  "Deletes or closes the current Project and takes the user
back to the starting popup."

  ;; If no status is provided, delete the Project, rather than close it.
  (if (eq status nil)
      (delete-directory-and-files *top-dir*))
  (setf *top-dir* "")
  (setf *current-dir* "")

  (disconnect *db*)
  (setf *db* "")

  (setf *entered-main-interface* 0)
  (setf *inv-count* 0)
  (setf *spaces-count* 0)

  (makunbound '*space_name*)
  (makunbound '*space_photo*)
  (makunbound '*current-item*)
  (makunbound '*current-item-name*))

(defun writable-and-new-p (file)
  "Checks whether a file exists and whether it can be created there."
  (and
   (not (file-exists-p file))
   (ignore-errors (open file :direction :probe :if-does-not-exist :create))
   (ignore-errors(delete-file file))))

(defun prepare-main-photo (photo_object button1 button2 photo_file)
  "Performs the necessary actions to set a main space photo."
  (setf (button-label button1) "Select another photo")
  (setf (widget-sensitive button2) t)

  ;; Calls Imagemagick to resize the photo that was provided.
  (run-shell-command (concatenate 'string "convert '"
				  photo_file "' -resize 501x301! '" *current-dir* "space_photo'"))
  (let ((temp_pixbuf
	 (pixbuf-new-from-file (concatenate 'string *current-dir* "space_photo"))))
    (setf (image-pixbuf photo_object) temp_pixbuf)))

(defun select-main-photo (photo button1 button2)
  "Creates a file chooser dialog for the user to select a space photo."
  (let ((photo_dialog
	 (make-instance 'file-chooser-dialog
			:title "Select a photo for this space"
			:action :open
			:local-only t))
	(image_filter (make-instance 'file-filter
				     :name "Image files (*.png, *.jpg, *.jpeg, *.tif, *.tiff, *.bmp)")))

    ;; Make sure that only image files are returned.
    (file-filter-add-pixbuf-formats image_filter)
    (file-chooser-add-filter photo_dialog image_filter)

    (dialog-add-button photo_dialog "gtk-cancel" :cancel)
    (dialog-add-button photo_dialog "gtk-ok" :ok)

    (connect-signal photo_dialog "response"
		    #'(lambda (photo_dialog response)

			;; Action when Cancel is pressed.
			(cond ((eq response -6)
			       (object-destroy photo_dialog))

			      ;; Actions when OK is pressed.
			      ((eq response -5)
			       (if (readable-p (format nil "~{~A~}"
						       (file-chooser-filenames photo_dialog)))
				   (progn
				     (prepare-main-photo photo button1 button2
							 (format nil "~{~A~}" (file-chooser-filenames photo_dialog)))
				     (object-destroy photo_dialog))
				 (failure "space_photo"))))))

    (widget-show photo_dialog)))

(defun make-inventory-entry (name photo_field description description_field
				  table remove_button)
  "Creates the new item entry in the inventory."
  (let ((event_box (make-instance 'event-box))
	(hbox (make-instance 'h-box))
	(event_label (make-instance 'label :label name)))

    ;; Create an event box containing the item's name.
    (box-pack-start hbox event_label :expand nil)
    (container-add event_box hbox)
    (connect-signal event_box "button_press_event"
		    #'(lambda (a b)
			(declare (ignorable b))
			(setf *current-item* a)
			(setf *current-item-name* name)

			;; Show the item's photo.
			(setf (image-file photo_field)
			      (concatenate 'string *current-dir* "items/" name))

			;; Show the item's description (if any).
			(setf (text-buffer-text
			       (text-view-buffer description_field)) description)
			(setf (widget-sensitive remove_button) t)))

    ;; Alternate between two colors for the event box background.
    (if (evenp *inv-count*)
	(widget-modify-bg event_box 0 (color-parse "#FFFFC2"))
      (widget-modify-bg event_box 0 (color-parse "#E0E4E4")))

    ;; Add the item to the table and increase the inventory count by 1.
    (table-attach table event_box 0 1 *inv-count* (+ *inv-count* 1))
    (incf *inv-count*)

    (widget-show table)))

(defun add-to-inventory (name photo description inventory photo_field
			      description_field table remove_button)
  "Performs the necessary actions to add a new item to the inventory."

  ;; Insert the item's details into the sqlite3 database.
  (execute-non-query *db*
		     (concatenate 'string "insert into '" *space_name*
				  "' (item, location, description) values (?, ?, ?)")
		     name *space_name* description)

  ;; Use Imagemagick to resize the item's photo.
  (run-shell-command (concatenate 'string "convert '"
				  photo "' -resize 202x102! '" *current-dir*
				  "items/" name "'"))

  (setf (widget-sensitive inventory) t)

  (make-inventory-entry name photo_field description description_field
			table remove_button))

(defun rename-space (button window spaces_table name photo button1 button2
			    spaces_list vbox item_image item_description
			    item_table remove_button item_vbox inventory_list
			    item_hbox)
  "Renames a space."
  (let ((dialog (make-instance 'message-dialog
			       :message-type :other
			       :buttons :ok-cancel
			       :text "Enter a new name for this space:"))
	(entry (make-instance 'entry
			      :text (button-label button)
			      :max-length 100)))

    (container-add (dialog-content-area dialog) entry)

    (connect-signal dialog "response"
		    #'(lambda (dialog response)

			;; Action when Cancel is pressed.
			(cond ((eq response -6)
			       (object-destroy dialog))

			      ;; Actions when OK is pressed.
			      ((eq response -5)
			       (if (or

				    ;; Make sure that the name does not contain numbers.
				    (not (eq
					  (scan "\\d" (entry-text entry)) nil))

				    ;; Make sure that the name only contains
				    ;; word characters and underscores.
				    (eq (scan
					 "^\\w+$" (entry-text entry)) nil)

				    ;; Make sure that the name did change.
				    (string-equal (entry-text entry) *space_name*)

				    (string-equal (entry-text entry)
						  (concatenate 'string *space_name* "/"))

				    ;; Make sure that the target directory
				    ;; does not exist.
				    (directory-exists-p
				     (concatenate 'string *top-dir*
						  (entry-text entry) "/")))
				   (failure "modify-name")
				 (progn
				   (rename-file *current-dir*
						(concatenate 'string *top-dir*
							     (entry-text entry) "/"))

				   ;; Rename the sqlite3 table.
				   (execute-non-query *db*
						      (concatenate 'string "alter table '"
								   *space_name* "' rename to '"
								   (entry-text entry) "';"))

				   (setf *current-dir* (concatenate 'string *top-dir*
								    (entry-text entry) "/"))

				   (setf *space_name* (entry-text entry))
				   (setf (button-label button) (entry-text entry))

				   (setf *spaces-count* 0)

				   ;; Empty the spaces list and re-populate it.
				   (map-container-children spaces_table #'object-destroy)
				   (list-existing-spaces spaces_table window name photo button1
							 button2 spaces_list item_image
							 item_description item_table
							 remove_button item_vbox inventory_list
							 item_hbox)
				   (widget-show vbox)
				   (object-destroy dialog)))))))

    (widget-show dialog)))

(defun remove-item (button image description)
  "Removes the selected item from the inventory."

  ;; Make sure that an item is selected.
  (if (boundp '*current-item*)
      (progn
	(object-destroy *current-item*)

	;; Delete the item's photo file.
	(delete-file (concatenate 'string *current-dir* "items/"
				  *current-item-name*))

	;; Remove the item from the database.
	(execute-non-query *db*
			   (concatenate 'string "delete from '" *space_name*
					"' where item = '" *current-item-name* "'"))

	(makunbound '*current-item*)
	(makunbound '*current-item-name*)

	;; Restore the default behavior.
	(setf (image-file image) "./images/default_item.png")
	(setf (text-buffer-text (text-view-buffer description)) "")
	(setf (widget-sensitive button) nil))))

(defun export-project ()
  "Creates a dialog for exporting the current Project."
  (let ((dialog
	 (make-instance 'file-chooser-dialog
			:title "Export the current project to a file"
			:action :save
			:current-name "sisor_project.tar.gz"
			:local-only t))
	(file_filter (make-instance 'file-filter
				    :name "Compressed tar archives (*.tar.gz)"))
	(filename ""))

    ;; Make sure that only *.tar.gz files are shown.
    (file-filter-add-pattern file_filter "*.tar.gz")
    (file-chooser-add-filter dialog file_filter)

    (dialog-add-button dialog "gtk-cancel" :cancel)
    (dialog-add-button dialog "gtk-ok" :ok)

    (connect-signal dialog "response"
		    #'(lambda (dialog response)

			;; Action when Cancel is pressed.
			(cond ((eq response -6)
			       (object-destroy dialog))

			      ;; Actions when OK is pressed.
			      ((eq response -5)
			       (setf filename (file-chooser-filename dialog))

			       ;; If the filename doesn't end with ".tar.gz",
			       ;; add the extension.
			       (if (eq (scan "[.]tar[.]gz$" filename) nil)
				   (setf filename
					 (concatenate 'string filename ".tar.gz")))

			       ;; Make sure that the file doesn't exist and can
			       ;; be created.
			       (if (writable-and-new-p filename)
				   (progn

				     ;; Create a compressed archive with tar.
				     (run-shell-command
				      (concatenate 'string "tar czf '" filename
						   "' -C '" *top-dir* "../' '"
						   (scan-to-strings "[^/]*/$" *top-dir*)
						   "'"))
				     (object-destroy dialog))
				 (failure "export"))))))

    (widget-show dialog)))

(defun exists-in-db-p (item)
  "Checks whether an item already exists by that name in the database."
  (not (endp (execute-to-list *db*
			      (concatenate 'string "select item from '"
					   *space_name* "' where item = ?") item))))

(defun switch-space (name_button name_photo button1 button2 item_image
				 item_description item_table remove_button vbox
				 inventory_list item_hbox)
  "Switches the active space."
  (setf (button-label name_button) *space_name*)

  ;; If the space does have a photo, load it or use the default one.
  (if (file-exists-p
       (concatenate 'string *current-dir* "space_photo"))
      (progn
	(setf *space_photo*
	      (concatenate 'string *current-dir* "space_photo"))
	(setf (image-file name_photo) *space_photo*)
	(setf (button-label button1) "Select another photo")
	(setf (widget-sensitive button2) t))
    (progn
      (makunbound '*space_photo*)
      (setf (image-file name_photo) "./images/default_space.png")
      (setf (button-label button1) "Select a photo for this space")
      (setf (widget-sensitive button2) nil)))

  ;; Clear the inventory list and get the available items.
  (setf *inv-count* 0)
  (map-container-children item_table #'object-destroy)
  (get-inventory-items item_image item_description item_table
		       remove_button vbox inventory_list)

  ;; Clear the description field.
  (setf (text-buffer-text (text-view-buffer item_description)) "")

  (widget-show item_hbox))

(defun add-space (table window name_button space_photo button1 button2
			item_image item_description item_table remove_button
			vbox inventory_list item_hbox)
  "Allows the user to enter a name for a new space."
  (let ((dialog (make-instance 'message-dialog
			       :message-type :other
			       :buttons :ok-cancel
			       :text "Enter a name for the new space:"))
	(entry (make-instance 'entry
			      :max-length 100)))

    (container-add (dialog-content-area dialog) entry)

    (connect-signal dialog "response"
		    #'(lambda (dialog response)

			;; Action when Cancel is pressed.
			(cond ((eq response -6)
			       (object-destroy dialog))

			      ;; Actions when OK is pressed.
			      ((eq response -5)
			       (if (or

				    ;; Make sure that the name does not contain numbers.
				    (not (eq
					  (scan "\\d" (entry-text entry)) nil))

				    ;; Make sure that the name only contains
				    ;; word characters and underscores.
				    (eq (scan
					 "^\\w+$" (entry-text entry)) nil)

				    ;; Make sure that the target directory does
				    ;; not exist.
				    (directory-exists-p
				     (concatenate 'string *top-dir*
						  (entry-text entry) "/")))
				   (failure "add-space")
				 (progn

				   ;; Create the new space and switch to it.
				   (create-space (entry-text entry))
				   (make-space-entry "space" window table
						     :name (entry-text entry)
						     :name_button name_button
						     :space_photo space_photo
						     :button1 button1
						     :button2 button2
						     :item_image item_image
						     :item_description item_description
						     :item_table item_table
						     :remove_button remove_button
						     :vbox vbox
						     :inventory_list inventory_list
						     :item_hbox item_hbox)
				   (setf *space_name* (entry-text entry))

				   (switch-space name_button space_photo button1
						 button2 item_image item_description item_table
						 remove_button vbox inventory_list item_hbox)

				   (object-destroy dialog)))))))

    (widget-show dialog)))

(defun make-space-entry (status window table &key name counter name_button
				space_photo button1 button2 item_image item_description
				item_table remove_button vbox inventory_list item_hbox)
  "Adds a new space to the list of existing ones."
  (cond
   ((string-equal status "space")

    ;; If this is the first item, put in the "Add new space" line first.
    (if (eql *spaces-count* 0)
	(make-space-entry "first_item" window table
			  :name_button name_button
			  :space_photo space_photo
			  :button1 button1
			  :button2 button2
			  :item_image item_image
			  :item_description item_description
			  :item_table item_table
			  :remove_button remove_button
			  :vbox vbox
			  :inventory_list inventory_list
			  :item_hbox item_hbox))

    (let ((hbox (make-instance 'h-box :spacing 10))
	  (image_box (make-instance 'event-box))
	  (image (make-instance 'image :stock "gtk-remove"))
	  (name_box (make-instance 'event-box))
	  (name_label (make-instance 'label :label name)))

      (box-pack-start hbox (make-instance 'label :label "") :expand nil)

      ;; Delete the space if the '-' image is pressed.
      (container-add image_box image)
      (connect-signal image_box "button_press_event"
		      #'(lambda (a b)
			  (declare (ignorable a b))
			  (delete-reask "space" window
					:name name
					:hbox hbox
					:name_button name_button
					:space_photo space_photo
					:button1 button1
					:button2 button2
					:item_image	item_image
					:item_description item_description
					:table item_table
					:remove_button remove_button
					:vbox vbox
					:inventory_list inventory_list
					:item_hbox item_hbox)))
      (box-pack-start hbox image_box :expand nil)

      ;; Switch to the space if its name is pressed.
      (container-add name_box name_label)
      (connect-signal name_box "button_press_event"
		      #'(lambda (a b)
			  (declare (ignorable a b))
			  (setf *current-dir*
				(concatenate 'string *top-dir* name "/"))

			  (setf *space_name* name)
			  (switch-space name_button space_photo button1 button2
					item_image item_description item_table remove_button
					vbox inventory_list item_hbox)))
      (box-pack-start hbox name_box :expand nil)

      (table-attach table hbox 0 1 *spaces-count* (+ *spaces-count* 1))
      (incf *spaces-count*)
      (widget-show table)))

   ;; Put in the new space adding item.
   ((string-equal status "first_item")
    (let ((event_box (make-instance 'event-box))
	  (hbox (make-instance 'h-box :spacing 10))
	  (image (make-instance 'image :stock "gtk-add"))
	  (name_label (make-instance 'label
				     :label "<b>Add new space</b>"
				     :use-markup t)))

      (box-pack-start hbox (make-instance 'label :label "") :expand nil)
      (box-pack-start hbox image :expand nil)
      (box-pack-start hbox name_label :expand nil)
      (container-add event_box hbox)

      ;; Set its background to white.
      (widget-modify-bg event_box 0 (color-parse "#FFFFFF"))

      ;; Add a new space, when pressed.
      (connect-signal event_box "button_press_event"
		      #'(lambda (a b)
			  (declare (ignorable a b))
			  (add-space table window name_button space_photo button1
				     button2 item_image item_description item_table remove_button
				     vbox inventory_list item_hbox)))

      (table-attach table event_box 0 1 *spaces-count* (+ *spaces-count* 1))
      (incf *spaces-count*)
      (widget-show table)))

   ((string-equal status "dummy")
    (cond
     ((eql counter 30) (widget-show table))
     (t (let ((dummy_label (make-instance 'label :label "")))
	  (table-attach table dummy_label 0 1 counter (+ counter 1)))

	(make-space-entry "dummy" window table :counter (+ counter 1)))))))

(defun list-existing-spaces (spaces_table window name photo button1 button2
					  spaces_list item_image item_description
					  item_table remove_button vbox inventory_list
					  item_hbox)
  "Creates the list of existing spaces."
  (dolist (item (sort (execute-to-list *db*
				       "select name from sqlite_master where type = 'table'")
		      #'string< :key #'car))

    (setf item (car item))

    (make-space-entry "space" window spaces_table
		      :name item
		      :name_button name
		      :space_photo photo
		      :button1 button1
		      :button2 button2
		      :item_image item_image
		      :item_description item_description
		      :item_table item_table
		      :remove_button remove_button
		      :vbox vbox
		      :inventory_list inventory_list
		      :item_hbox item_hbox))

  (make-space-entry "dummy" window spaces_table :counter *spaces-count*)
  (scrolled-window-add-with-viewport spaces_list spaces_table))

(defun get-inventory-items (image description table remove_button vbox
				  inventory_list)
  "Retrieves the inventory items for the current space."
  (setf (image-file image) "./images/default_item.png")
  (dolist (item (sort (execute-to-list *db*
				       (concatenate 'string "select item from '"
						    *space_name* "'"))
		      #'string< :key #'car))

    (setf item (car item))

    (make-inventory-entry item image
			  (caar (execute-to-list *db*
						 (concatenate 'string "select description from '"
							      *space_name* "' where item='" item "'")))
			  description table remove_button))

  ;; If no items are detected, disable the inventory area.
  (if (> *inv-count* 0)
      (setf (widget-sensitive vbox) t)
    (setf (widget-sensitive vbox) nil))

  (scrolled-window-add-with-viewport inventory_list table))

(defun main-interface ()
  "Creates the main interface of the application."
  (within-main-loop
   (let ((window (make-instance 'gtk-window
				:title "Sisor"
				:icon (image-pixbuf
				       (make-instance 'image
						      :file "./images/sisor_minilogo.png"))
				:type :toplevel
				:window-position :center
				:resizable nil))

	 (vbox1 (make-instance 'v-box :spacing 5))
	 (hbox1 (make-instance 'h-box))
	 (toolbar (make-instance 'toolbar))
	 (event_new (make-instance 'event-box))
	 (new_vbox (make-instance 'v-box))
	 (new_button (make-instance 'image
				    :stock "gtk-new"
				    :icon-size 3))
	 (event_open (make-instance 'event-box))
	 (open_vbox (make-instance 'v-box))
	 (open_button (make-instance 'image
				     :stock "gtk-open"
				     :icon-size 3))
	 (event_delete (make-instance 'event-box))
	 (delete_vbox (make-instance 'v-box))
	 (delete_button (make-instance 'image
				       :stock "gtk-delete"
				       :icon-size 3))
	 (event_about (make-instance 'event-box))
	 (about_vbox (make-instance 'v-box))
	 (about_button (make-instance 'image
				      :stock "gtk-about"
				      :icon-size 3))
	 (event_exit (make-instance 'event-box))
	 (exit_vbox (make-instance 'v-box))
	 (exit_button (make-instance 'image
				     :stock "gtk-quit"
				     :icon-size 3))
	 (hbox2 (make-instance 'h-box
			       :spacing 5))
	 (vbox2 (make-instance 'v-box :spacing 5))
	 (space_name_hbox (make-instance 'h-box :spacing 20))
	 (space_name (make-instance 'button
				    :label (check-defined "space_name")))
	 (space_photo (make-instance 'image
				     :file (check-defined "space_photo")))
	 (select_hbox (make-instance 'h-box))
	 (select_button (make-instance 'button))
	 (delete_image (make-instance 'button :label "Remove the photo"))
	 (managing_hbox (make-instance 'h-box
				       :spacing 20))
	 (inventory_vbox (make-instance 'v-box :sensitive nil :spacing 5))
	 (inventory_hbox (make-instance 'h-box))
	 (inventory (make-instance 'label
				   :label "<b>  Inventory:    </b>"
				   :use-markup t))
	 (inventory_list (make-instance 'scrolled-window
					:height-request 45
					:width-request 175))
	 (inventory_table (make-instance 'table))
	 (item_image (make-instance 'image
				    :file "./images/default_item.png"))
	 (item_description (make-instance 'text-view
					  :editable t
					  :wrap-mode :word-char
					  :height-request 60
					  :width-request 170))
	 (item_actions_hbox (make-instance 'h-box))
	 (remove_item (make-instance 'button
				     :label "Remove"
				     :sensitive nil))
	 (move_item (make-instance 'button
				   :label "Move to another space"
				   :sensitive nil))
	 (add_item_vbox (make-instance 'v-box
				       :spacing 23))
	 (add_item (make-instance 'label
				  :label "<b>Add new item</b>"
				  :use-markup t))
	 (new_item_hbox (make-instance 'h-box :spacing 10))
	 (image_file_label (make-instance 'label
					  :label "Image file:"))
	 (item_select_button (make-instance 'file-chooser-button
					    :width-request 150))
	 (naming_hbox (make-instance 'h-box
				     :spacing 10))
	 (new_item_name (make-instance 'label
				       :label "Item name:"))
	 (item_entry (make-instance 'entry))
	 (description_hbox (make-instance 'h-box :spacing 10))
	 (description_label (make-instance 'label
					   :label "Description:"))
	 (description (make-instance 'text-view
				     :editable t
				     :wrap-mode :word-char
				     :height-request 60
				     :width-request 170))
	 (add_button_hbox (make-instance 'h-box :spacing 10))
	 (add_button (make-instance 'button
				    :label "Add to inventory"))
	 (side_vbox (make-instance 'v-box))
	 (other_spaces (make-instance 'label
				      :label "      <b>Spaces in this project</b>      "
				      :use-markup t))
	 (spaces_list (make-instance 'scrolled-window))
	 (spaces_table (make-instance 'table)))

     ;; Create the menu.
     (container-add vbox1 (create-menubar window))

     ;; Create the icon toolbar.
     (container-add vbox1 toolbar)
     (container-add toolbar hbox1)

     ;; New Project.
     (container-add new_vbox new_button)
     (container-add new_vbox
		    (make-instance 'label :label "   New Project   "))

     (container-add event_new new_vbox)
     (g-signal-connect event_new "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (get-project window "new")))

     (container-add hbox1 event_new)
     (container-add hbox1 (make-instance 'v-separator))

     ;; Open Project.
     (container-add open_vbox open_button)
     (container-add open_vbox
		    (make-instance 'label :label "   Open Project   "))

     (container-add event_open open_vbox)
     (g-signal-connect event_open "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (get-project window "open")))

     (container-add hbox1 event_open)
     (container-add hbox1 (make-instance 'v-separator))

     ;; Delete the current Project.
     (container-add delete_vbox delete_button)
     (container-add delete_vbox
		    (make-instance 'label :label "   Delete Project   "))

     (container-add event_delete delete_vbox)
     (g-signal-connect event_delete "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (delete-reask "project" window)))
     (container-add hbox1 event_delete)
     (container-add hbox1 (make-instance 'v-separator))

     ;; About Sisor.
     (container-add about_vbox about_button)
     (container-add about_vbox
		    (make-instance 'label :label "   About Sisor   "))

     (container-add event_about about_vbox)
     (g-signal-connect event_about "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (about window)))
     (container-add hbox1 event_about)
     (container-add hbox1 (make-instance 'v-separator))

     ;; Exit Sisor.
     (container-add exit_vbox exit_button)
     (container-add exit_vbox
		    (make-instance 'label :label "   Exit Sisor   "))

     (container-add event_exit exit_vbox)
     (g-signal-connect event_exit "button_press_event"
		       #'(lambda (a b)
			   (declare (ignorable a b))
			   (object-destroy window)
			   (disconnect *db*)
			   (exit :abort t)))
     (container-add hbox1 event_exit)
     (container-add hbox1 (make-instance 'v-separator))

     (container-add vbox1 (make-instance 'h-separator))

     ;; Space name button and renaming it.
     (container-add space_name_hbox (make-instance 'label
						   :label "               "))

     (g-signal-connect space_name "clicked"
		       #'(lambda (button)
			   (rename-space button window spaces_table space_name
					 space_photo select_button delete_image spaces_list
					 side_vbox item_image item_description
					 inventory_table remove_item inventory_vbox
					 inventory_list inventory_hbox)))
     (container-add space_name_hbox space_name)

     (container-add space_name_hbox (make-instance 'label
						   :label "               "))

     (container-add vbox2 space_name_hbox)
     (container-add vbox2 space_photo)

     (container-add select_hbox (make-instance 'label
					       :label "             "))

     ;; Show the photo if the space already has a main one.
     (if (boundp '*space_photo*)
	 (progn
	   (setf (button-label select_button) "Select another photo")
	   (setf (widget-sensitive delete_image) t))
       (progn
	 (setf (button-label select_button) "Select a photo for this space")
	 (setf (widget-sensitive delete_image) nil)))

     ;; Select main photo button.
     (g-signal-connect select_button "clicked"
		       #'(lambda (button)
			   (select-main-photo space_photo button delete_image)))
     (container-add select_hbox select_button)

     ;; Button that removes the main photo.
     (g-signal-connect delete_image "clicked"
		       #'(lambda (b)
			   (declare (ignorable b))
			   (delete-file
			    (concatenate 'string *current-dir*  "space_photo"))
			   (makunbound '*space_photo*)
			   (setf (widget-sensitive delete_image) nil)
			   (setf (button-label select_button) "Select a photo for this space")
			   (setf (image-file space_photo) "./images/default_space.png")))
     (container-add select_hbox delete_image)

     (container-add select_hbox (make-instance 'label
					       :label "             "))

     (container-add vbox2 select_hbox)

     (container-add vbox2 (make-instance 'h-separator))

     ;; Inventory.
     (container-add inventory_hbox inventory)

     ;; Get the existing items (if any).
     (get-inventory-items item_image item_description inventory_table
			  remove_item inventory_vbox inventory_list)

     (container-add inventory_hbox inventory_list)

     (container-add inventory_vbox inventory_hbox)

     (container-add inventory_vbox item_image)

     (container-add inventory_vbox item_description)

     ;; Remove a specific item.
     (g-signal-connect remove_item "clicked"
		       #'(lambda (a)
			   (declare (ignorable a))
			   (remove-item remove_item item_image
					item_description)))
     (container-add item_actions_hbox remove_item)

     ;; Move a specific item to another space. (NOT WORKING YET. TODO.)
     (container-add item_actions_hbox move_item)
     (container-add inventory_vbox item_actions_hbox)

     ;; Add new item area.
     (container-add managing_hbox inventory_vbox)
     (container-add managing_hbox (make-instance 'v-separator))

     (container-add add_item_vbox add_item)

     (container-add new_item_hbox image_file_label)

     ;; Set the image filter to the image selecting button.
     (let ((image_filter (make-instance 'file-filter
					:name "Image files (*.png, *.jpg, *.jpeg, *.tif, *.tiff, *.bmp)")))

       (file-filter-add-pixbuf-formats image_filter)
       (file-chooser-add-filter item_select_button image_filter))

     (container-add new_item_hbox item_select_button)
     (container-add add_item_vbox new_item_hbox)

     ;; Define a name for the new item.
     (container-add naming_hbox new_item_name)
     (container-add naming_hbox item_entry)
     (container-add add_item_vbox naming_hbox)

     ;; Enter an optional description for the item.
     (container-add description_hbox description_label)
     (container-add description_hbox description)
     (container-add add_item_vbox description_hbox)

     (container-add add_button_hbox (make-instance 'label
						   :label "        "))

     ;; Make sure that the item has an appropriate and unique name,
     ;; and a photo.
     (g-signal-connect add_button "clicked"
		       #'(lambda (b)
			   (declare (ignorable b))
			   (if (or (not (eq (scan "^\\s*$"
						  (entry-text item_entry)) nil))
				   (eq (scan "png|PNG|jpg||JPG|jpeg|JPEG|gif|GIF$"
					     (file-chooser-filename item_select_button)) nil)
				   (exists-in-db-p (entry-text item_entry))
				   (not (readable-p (file-chooser-filename item_select_button))))
			       (failure "space_item")

			     ;; Add the item to the inventory.
			     (add-to-inventory
			      (entry-text item_entry)
			      (file-chooser-filename item_select_button)
			      (text-buffer-text (text-view-buffer description))
			      inventory_vbox
			      item_image
			      item_description
			      inventory_table
			      remove_item))))

     (container-add add_button_hbox add_button)
     (container-add add_button_hbox (make-instance 'label
						   :label "        "))

     (container-add add_item_vbox add_button_hbox)

     (container-add managing_hbox add_item_vbox)

     (container-add vbox2 managing_hbox)

     (container-add hbox2 vbox2)
     (container-add hbox2 (make-instance 'v-separator))

     ;; Other spaces within this Project.
     (box-pack-start side_vbox other_spaces :expand nil)

     (box-pack-start side_vbox (make-instance 'h-separator)
		     :expand nil)

     ;; Create the list of the available spaces.
     (list-existing-spaces spaces_table window space_name space_photo
			   select_button delete_image spaces_list item_image
			   item_description inventory_table remove_item
			   inventory_vbox inventory_list inventory_hbox)

     (container-add side_vbox spaces_list)
     (container-add hbox2 side_vbox)

     (container-add vbox1 hbox2)

     (container-add window vbox1)

     (g-signal-connect window "destroy"
		       #'(lambda (b)
			   (declare (ignorable b))
			   (if (eq *entered-main-interface* 1)
			       (progn
				 (disconnect *db*)
				 (exit :abort t)))))
     (widget-show window :all :t))))

