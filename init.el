;; -*- lexical-binding: t -*-

(eval-and-compile
  (add-to-list 'load-path (in-config-directory "lib/borg"))
  (require 'borg)
  (borg-initialize))
