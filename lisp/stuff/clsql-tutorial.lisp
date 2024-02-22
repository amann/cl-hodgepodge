(defpackage #:clsql-tutorial
  (:use #:cl #:clsql))
(in-package :clsql-tutorial)




(clsql:def-view-class employee ()
  ((emplid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :emplid)
   (first-name
    :accessor first-name
    :type (string 30)
    :initarg :first-name)
   (last-name
    :accessor last-name
    :type (string 30)
    :initarg :last-name)
   (email
    :accessor employee-email
    :type (string 100)
    :nulls-ok t
    :initarg :email)
   (companyid
    :type integer
    :initarg :companyid)
   (managerid
    :type integer
    :nulls-ok t
    :initarg :managerid)
   (manager
    :accessor employee-manager
    :db-kind :join
    :db-info (:join-class employee
			  :home-key managerid
			  :foreign-key emplid
			  :set nil)))
  (:base-table employee)
  (:documentation "Corresponds to the SQL statement:
CREATE TABLE EMPLOYEE (emplid NOT NULL number(38),
		       first_name NOT NULL varchar2(30),
		       last_name NOT NULL varchar2(30),
		       email varchar2(100),
		       companyid NOT NULL number(38),
		       managerid number(38))"))

(clsql:def-view-class company ()
  ((companyid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :companyid)
   (name
    :type (string 100)
    :initarg :name)
   (presidentid
    :type integer
    :initarg :presidentid)
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class employee
	       :home-key companyid
	       :foreign-key companyid
	       :set t))
   (president
    :reader president
    :db-kind :join
    :db-info (:join-class employee
			  :home-key presidentid
			  :foreign-key emplid
			  :set nil)))
  (:base-table company)
  (:documentation "Corresponds to the SQL statement:
CREATE TABLE COMPANY (companyid NOT NULL number(38),
		      name NOT NULL varchar2(100),
		      presidentid NOT NULL number(38))"))

