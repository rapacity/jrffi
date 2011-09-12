#include <jni.h>
#include <stdarg.h>
#include <stdlib.h>

/*
   unsigned short dispatch_max;
   JNIEnv** envs;
   jmethodID* methods;
   jclass* classes;
*/

JavaVM* jvm;

int create_jvm(JavaVMOption* options, int n_options) {
  JNIEnv* env;
  JavaVMInitArgs args;
  args.options  = options;
  args.nOptions = n_options;
  args.ignoreUnrecognized = 0;
  args.version  = JNI_VERSION_1_6;
  return JNI_CreateJavaVM(&jvm, (void**)&env, &args);
}

void delete_local_ref(JNIEnv* env, jobject obj) {
  (*env)->DeleteLocalRef(env, obj);
}

jobject new_global_ref(JNIEnv* env, jobject obj) {
  (*env)->NewGlobalRef(env, obj);
}

jobject delete_global_ref(JNIEnv* env, jobject obj) {
  (*env)->DeleteGlobalRef(env, obj);
}

int is_jvm_initialized() {
  return (jvm != NULL);
}

JNIEnv* attach_current_thread() {
  JNIEnv* new_env;
  (*jvm)->AttachCurrentThread(jvm, (void**)&new_env, NULL);
  return new_env;
}

jint detach_current_thread() {
  return (*jvm)->DetachCurrentThread(jvm);
}

jint register_natives(JNIEnv* env, jclass class, const JNINativeMethod *methods,
                      jint n_methods) {
 (*env)->RegisterNatives(env, class, methods, n_methods);
}

jboolean exception_check(JNIEnv* env) {
  return (*env)->ExceptionCheck(env);
}

jthrowable exception_occurred(JNIEnv* env) {
  return (*env)->ExceptionOccurred(env);
}

void exception_clear(JNIEnv* env) {
  (*env)->ExceptionClear(env);
}

jclass find_class(JNIEnv* env, const char* name) {
  return (*env)->FindClass(env, name);
}

jclass get_object_class(JNIEnv* env, jobject obj) {
  return (*env)->GetObjectClass(env, obj);
}

jboolean is_instance_of(JNIEnv* env, jobject obj, jclass class) {
  return (*env)->IsInstanceOf(env, obj, class);
}

jmethodID get_method_id(JNIEnv* env, jclass class, const char* name, const char* signature) {
  return (*env)->GetMethodID(env, class, name, signature);
}

jmethodID get_static_method_id(JNIEnv* env, jclass class, const char* name, const char* signature) {
  return (*env)->GetStaticMethodID(env, class, name, signature);
}

jfieldID get_field_id(JNIEnv* env, jclass class, const char* name, const char* signature) {
  return (*env)->GetFieldID(env, class, name, signature);
}

jfieldID get_static_field_id(JNIEnv* env, jclass class, const char* name, const char* signature) {
  return (*env)->GetStaticFieldID(env, class, name, signature);
}

jstring new_string(JNIEnv* env, const char* str) {
  return (*env)->NewStringUTF(env, str);
}


const char* get_string(JNIEnv* env, jstring str) {
  return (*env)->GetStringUTFChars(env, str, NULL);
}

jsize get_string_length(JNIEnv* env, jstring str) {
  return (*env)->GetStringLength(env, str);
}

void release_string_chars(JNIEnv* env, jstring str, const char* chars) {
  (*env)->ReleaseStringUTFChars(env, str, chars);
}

jobject new_object(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->NewObjectV(env, class, method, args);
}

void call_void_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallVoidMethodV(env, obj, method, args);
}

jobject call_object_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallObjectMethodV(env, obj, method, args);
}

jint call_int_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallIntMethodV(env, obj, method, args);
}

jshort call_short_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallShortMethodV(env, obj, method, args);
}

jlong call_long_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallLongMethodV(env, obj, method, args);
}

jboolean call_boolean_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallBooleanMethodV(env, obj, method, args);
}

jbyte call_byte_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallByteMethodV(env, obj, method, args);
}

jchar call_char_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallCharMethodV(env, obj, method, args);
}

jfloat call_float_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallFloatMethodV(env, obj, method, args);
}

jdouble call_double_method(JNIEnv* env, jobject obj, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallDoubleMethodV(env, obj, method, args);
}

void call_static_void_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticVoidMethodV(env, class, method, args);
}

jobject call_static_object_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticObjectMethodV(env, class, method, args);
}

jint call_static_int_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticIntMethodV(env, class, method, args);
}

jshort call_static_short_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticShortMethodV(env, class, method, args);
}

jlong call_static_long_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticLongMethodV(env, class, method, args);
}

jboolean call_static_boolean_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticBooleanMethodV(env, class, method, args);
}

jbyte call_static_byte_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticByteMethodV(env, class, method, args);
}

jchar call_static_char_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticCharMethodV(env, class, method, args);
}

jfloat call_static_float_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticFloatMethodV(env, class, method, args);
}

jdouble call_static_double_method(JNIEnv* env, jclass class, jmethodID method, ...) {
  va_list args;
  va_start(args, method);
  return (*env)->CallStaticDoubleMethodV(env, class, method, args);
}

jobject get_object_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetObjectField(env, obj, field);
}

jint get_int_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetIntField(env, obj, field);
}

jshort get_short_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetShortField(env, obj, field);
}

jlong get_long_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetLongField(env, obj, field);
}

jboolean get_boolean_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetBooleanField(env, obj, field);
}

jbyte get_byte_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetByteField(env, obj, field);
}

jchar get_char_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetCharField(env, obj, field);
}

jfloat get_float_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetFloatField(env, obj, field);
}

jdouble get_double_field(JNIEnv* env, jobject obj, jfieldID field) {
  return (*env)->GetDoubleField(env, obj, field);
}

jobject get_static_object_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticObjectField(env, class, field);
}

jint get_static_int_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticIntField(env, class, field);
}

jshort get_static_short_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticShortField(env, class, field);
}

jlong get_static_long_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticLongField(env, class, field);
}

jboolean get_static_boolean_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticBooleanField(env, class, field);
}

jbyte get_static_byte_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticByteField(env, class, field);
}

jchar get_static_char_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticCharField(env, class, field);
}

jfloat get_static_float_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticFloatField(env, class, field);
}

jdouble get_static_double_field(JNIEnv* env, jclass class, jfieldID field) {
  return (*env)->GetStaticDoubleField(env, class, field);
}

void set_object_field(JNIEnv* env, jobject obj, jfieldID field, jobject value) {
  (*env)->SetObjectField(env, obj, field, value);
}

void set_int_field(JNIEnv* env, jobject obj, jfieldID field, jint value) {
  (*env)->SetIntField(env, obj, field, value);
}

void set_short_field(JNIEnv* env, jobject obj, jfieldID field, jshort value) {
  (*env)->SetShortField(env, obj, field, value);
}

void set_long_field(JNIEnv* env, jobject obj, jfieldID field, jlong value) {
  (*env)->SetLongField(env, obj, field, value);
}

void set_boolean_field(JNIEnv* env, jobject obj, jfieldID field, jboolean value) {
  (*env)->SetBooleanField(env, obj, field, value);
}

void set_byte_field(JNIEnv* env, jobject obj, jfieldID field, jbyte value) {
  (*env)->SetByteField(env, obj, field, value);
}

void set_char_field(JNIEnv* env, jobject obj, jfieldID field, jchar value) {
  (*env)->SetCharField(env, obj, field, value);
}

void set_float_field(JNIEnv* env, jobject obj, jfieldID field, jfloat value) {
  (*env)->SetFloatField(env, obj, field, value);
}

void set_double_field(JNIEnv* env, jobject obj, jfieldID field, jdouble value) {
  (*env)->SetDoubleField(env, obj, field, value);
}

void set_static_object_field(JNIEnv* env, jclass class, jfieldID field, jobject value) {
  (*env)->SetStaticObjectField(env, class, field, value);
}

void set_static_int_field(JNIEnv* env, jclass class, jfieldID field, jint value) {
  (*env)->SetStaticIntField(env, class, field, value);
}

void set_static_short_field(JNIEnv* env, jclass class, jfieldID field, jshort value) {
  (*env)->SetStaticShortField(env, class, field, value);
}

void set_static_long_field(JNIEnv* env, jclass class, jfieldID field, jlong value) {
  (*env)->SetStaticLongField(env, class, field, value);
}

void set_static_boolean_field(JNIEnv* env, jclass class, jfieldID field, jboolean value) {
  (*env)->SetStaticBooleanField(env, class, field, value);
}

void set_static_byte_field(JNIEnv* env, jclass class, jfieldID field, jbyte value) {
  (*env)->SetStaticByteField(env, class, field, value);
}

void set_static_char_field(JNIEnv* env, jclass class, jfieldID field, jchar value) {
  (*env)->SetStaticCharField(env, class, field, value);
}

void set_static_float_field(JNIEnv* env, jclass class, jfieldID field, jfloat value) {
  (*env)->SetStaticFloatField(env, class, field, value);
}

void set_static_double_field(JNIEnv* env, jclass class, jfieldID field, jdouble value) {
  (*env)->SetStaticDoubleField(env, class, field, value);
}

jobjectArray new_object_array(JNIEnv* env, jsize len, jclass clazz, jobject init) {
  return (*env)->NewObjectArray(env, len, clazz, NULL);
}

void set_object_array_element(JNIEnv* env, jobjectArray arr, jsize index, jobject element) {
  (*env)->SetObjectArrayElement(env, arr, index, element);
}

jobject get_object_array_element(JNIEnv* env, jobjectArray arr, jsize index) {
  return (*env)->GetObjectArrayElement(env, arr, index);
}

jsize get_array_length(JNIEnv* env, jarray array) {
  return (*env)->GetArrayLength(env, array);
}

jintArray new_int_array(JNIEnv* env, jsize len) {
  return (*env)->NewIntArray(env, len);
}
void set_int_array_element(JNIEnv* env, jintArray arr, jsize index, jint element) {
  (*env)->SetIntArrayRegion(env, arr, index, 1, &element);
}
jint get_int_array_element(JNIEnv* env, jintArray arr, jsize index) {
  jint region;
  (*env)->GetIntArrayRegion(env, arr, index, 1, &region);
  return region;
}

jshortArray new_short_array(JNIEnv* env, jsize len) {
  return (*env)->NewShortArray(env, len);
}
void set_short_array_element(JNIEnv* env, jshortArray arr, jsize index, jshort element) {
  (*env)->SetShortArrayRegion(env, arr, index, 1, &element);
}
jshort get_short_array_element(JNIEnv* env, jshortArray arr, jsize index) {
  jshort region;
  (*env)->GetShortArrayRegion(env, arr, index, 1, &region);
  return region;
}

jlongArray new_long_array(JNIEnv* env, jsize len) {
  return (*env)->NewLongArray(env, len);
}
void set_long_array_element(JNIEnv* env, jlongArray arr, jsize index, jlong element) {
  (*env)->SetLongArrayRegion(env, arr, index, 1, &element);
}
jlong get_long_array_element(JNIEnv* env, jlongArray arr, jsize index) {
  jlong region;
  (*env)->GetLongArrayRegion(env, arr, index, 1, &region);
  return region;
}

jbooleanArray new_boolean_array(JNIEnv* env, jsize len) {
  return (*env)->NewBooleanArray(env, len);
}
void set_boolean_array_element(JNIEnv* env, jbooleanArray arr, jsize index, jboolean element) {
  (*env)->SetBooleanArrayRegion(env, arr, index, 1, &element);
}
jboolean get_boolean_array_element(JNIEnv* env, jbooleanArray arr, jsize index) {
  jboolean region;
  (*env)->GetBooleanArrayRegion(env, arr, index, 1, &region);
  return region;
}

jbyteArray new_byte_array(JNIEnv* env, jsize len) {
  return (*env)->NewByteArray(env, len);
}
void set_byte_array_element(JNIEnv* env, jbyteArray arr, jsize index, jbyte element) {
  (*env)->SetByteArrayRegion(env, arr, index, 1, &element);
}
jbyte get_byte_array_element(JNIEnv* env, jbyteArray arr, jsize index) {
  jbyte region;
  (*env)->GetByteArrayRegion(env, arr, index, 1, &region);
  return region;
}

jcharArray new_char_array(JNIEnv* env, jsize len) {
  return (*env)->NewCharArray(env, len);
}
void set_char_array_element(JNIEnv* env, jcharArray arr, jsize index, jchar element) {
  (*env)->SetCharArrayRegion(env, arr, index, 1, &element);
}
jchar get_char_array_element(JNIEnv* env, jcharArray arr, jsize index) {
  jchar region;
  (*env)->GetCharArrayRegion(env, arr, index, 1, &region);
  return region;
}

jfloatArray new_float_array(JNIEnv* env, jsize len) {
  return (*env)->NewFloatArray(env, len);
}
void set_float_array_element(JNIEnv* env, jfloatArray arr, jsize index, jfloat element) {
  (*env)->SetFloatArrayRegion(env, arr, index, 1, &element);
}
jfloat get_float_array_element(JNIEnv* env, jfloatArray arr, jsize index) {
  jfloat region;
  (*env)->GetFloatArrayRegion(env, arr, index, 1, &region);
  return region;
}

jdoubleArray new_double_array(JNIEnv* env, jsize len) {
  return (*env)->NewDoubleArray(env, len);
}
void set_double_array_element(JNIEnv* env, jdoubleArray arr, jsize index, jdouble element) {
  (*env)->SetDoubleArrayRegion(env, arr, index, 1, &element);
}
jdouble get_double_array_element(JNIEnv* env, jdoubleArray arr, jsize index) {
  jdouble region;
  (*env)->GetDoubleArrayRegion(env, arr, index, 1, &region);
  return region;
}

