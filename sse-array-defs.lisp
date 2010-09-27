;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;
;;; Copyright (c) 2010, Alexander Gavrilov (angavrilov@gmail.com)
;;;
;;; This file contains definitions for vectorized access
;;; to specialized lisp arrays.
;;;

(in-package #:SSE)

;;; Prefetch: AREF-PREFETCH-*, ROW-MAJOR-AREF-PREFETCH-*

(def-aref-intrinsic #:PREFETCH-T0 nil cpu-prefetch-t0 nil :check-bounds nil)
(def-aref-intrinsic #:PREFETCH-T1 nil cpu-prefetch-t1 nil :check-bounds nil)
(def-aref-intrinsic #:PREFETCH-T2 nil cpu-prefetch-t2 nil :check-bounds nil)
(def-aref-intrinsic #:PREFETCH-NTA nil cpu-prefetch-nta nil :check-bounds nil)

(def-aref-intrinsic #:CLFLUSH nil cpu-clflush nil :check-bounds :no-gap)

;;; Single-float

;; AREF-PS, ROW-MAJOR-AREF-PS

(def-aref-intrinsic #:PS float-sse-pack mem-ref-ps mem-set-ps)

;; AREF-APS, ROW-MAJOR-AREF-APS (requires alignment)

(def-aref-intrinsic #:APS float-sse-pack mem-ref-aps mem-set-aps)

;; AREF-SPS, ROW-MAJOR-AREF-SPS (requires alignment; no write cache)

(def-aref-intrinsic #:SPS float-sse-pack mem-ref-aps stream-ps)

;;; Double-float

;; AREF-PD, ROW-MAJOR-AREF-PD

(def-aref-intrinsic #:PD double-sse-pack mem-ref-pd mem-set-pd)

;; AREF-APD, ROW-MAJOR-AREF-APD (requires alignment)

(def-aref-intrinsic #:APD double-sse-pack mem-ref-apd mem-set-apd)

;; AREF-SPD, ROW-MAJOR-AREF-SPD (requires alignment; no write cache)

(def-aref-intrinsic #:SPD double-sse-pack mem-ref-apd stream-pd)

;;; Integer

;; AREF-PI, ROW-MAJOR-AREF-PI

(def-aref-intrinsic #:PI int-sse-pack mem-ref-pi mem-set-pi)

;; AREF-API, ROW-MAJOR-AREF-API (requires alignment)

(def-aref-intrinsic #:API int-sse-pack mem-ref-api mem-set-api)

;; AREF-SPI, ROW-MAJOR-AREF-SPI (requires alignment; no write cache)

(def-aref-intrinsic #:SPI int-sse-pack mem-ref-api stream-pi)

