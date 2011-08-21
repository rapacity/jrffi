#lang racket

(define (capitalize str)
  (let ([length (string-length str)])
    (if (zero? length) str
        (let ([first-char (string-ref str 0)])
          (if (char-alphabetic? first-char)
              (string-append (string (char-upcase first-char))
                             (substring str 1 length))
              str)))))

(define (call-generator return-type)
  (let ([return-type (format "~a" return-type)])
    (string-append
     "j" return-type " call_" return-type "_method(JNIEnv* env, jobject obj, jmethodID method, ...) {\n"
     "  va_list args;\n"
     "  va_start(args, method);\n"
     "  return (*env)->Call"  (capitalize return-type) "MethodV(env, obj, method, args);\n"
     "}\n")))

(define (call-static-generator return-type)
  (let ([return-type (format "~a" return-type)])
    (string-append
     "j" return-type " call_static_" return-type "_method(JNIEnv* env, jclass class, jmethodID method, ...) {\n"
     "  va_list args;\n"
     "  va_start(args, method);\n"
     "  return (*env)->CallStatic"  (capitalize return-type) "MethodV(env, class, method, args);\n"
     "}\n")))

(define (field-accessor-generator type)
  (let ([type (format "~a" type)])
    (string-append
     "j" type " get_" type "_field(JNIEnv* env, jobject obj, jfieldID field) {\n"
     "  return (*env)->Get" (capitalize type) "Field(env, obj, field);\n"
     "}\n")))

(define (static-field-accessor-generator type)
  (let ([type (format "~a" type)])
    (string-append
     "j" type " get_static_" type "_field(JNIEnv* env, jclass class, jfieldID field) {\n"
     "  return (*env)->GetStatic" (capitalize type) "Field(env, class, field);\n"
     "}\n")))

(define (field-mutator-generator type)
  (let ([type (format "~a" type)])
    (string-append
     "void set_" type "_field(JNIEnv* env, jobject obj, jfieldID field, j" type " value) {\n"
     "  (*env)->Set" (capitalize type) "Field(env, obj, field, value);\n"
     "}\n")))

(define (static-field-mutator-generator type)
  (let ([type (format "~a" type)])
    (string-append
     "void set_static_" type "_field(JNIEnv* env, jclass class, jfieldID field, j" type " value) {\n"
     "  (*env)->SetStatic" (capitalize type) "Field(env, class, field, value);\n"
     "}\n")))

(define (vector-access-generator type)
  (let* ([type (format "~a" type)]
         [Type (capitalize type)])
    (string-append
     "j" type "Array new_" type "_array(JNIEnv* env, jsize len) {\n"         
     "  return (*env)->New" Type "Array(env, len);\n"
     "}\n"
     "void set_" type "_array_element(JNIEnv* env, j" type "Array arr, jsize index, j" type " element) {\n"
     "  (*env)->Set" Type "ArrayRegion(env, arr, index, 1, &element);\n"
     "}\n"
     "j" type " get_" type "_array_element(JNIEnv* env, j" type "Array arr, jsize index) {\n"
     "  j" type " region;\n"
     "  (*env)->Get" Type "ArrayRegion(env, arr, index, 1, &region);\n"
     "  return region;\n"
     "}\n")))

(define types '(void object int short long boolean byte char float double))
(define field-types (remove 'void types))
(define primitive-types (remove 'object field-types))
;(for-each (compose displayln call-generator) types)
;(for-each (compose displayln call-static-generator) types)

;(for-each (compose displayln field-accessor-generator) field-types)
;(for-each (compose displayln static-field-accessor-generator) field-types)
;(for-each (compose displayln field-mutator-generator) field-types)
;(for-each (compose displayln static-field-mutator-generator) field-types)

(for-each (compose displayln vector-access-generator) primitive-types)

