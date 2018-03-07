// Libraries below are imported in SequentialCSharp.hs
// using System;
// using static System.Convert;
// using static System.Math;

// Scalar functions.
static sbyte signed(byte x){ return ToSByte(x);}
static short signed(ushort x){ return ToInt16(x);}
static int signed(uint x){ return ToInt32(x);}
static long signed(ulong x){ return ToInt64(x);}

static byte unsigned(sbyte x){ return ToByte(x);}
static ushort unsigned(short x){ return ToUInt16(x);}
static uint unsigned(int x){ return ToUInt32(x);}
static ulong unsigned(long x){ return ToUInt64(x);}

static sbyte shl8(sbyte x, sbyte y){ return ToSByte(x << y); }
static short shl16(short x, short y){ return ToInt16(x << y); }
static int shl32(int x, int y){ return x << y; }
static long shl64(long x, long y){ return x << ToInt32(y); }

static sbyte ashr8(sbyte x, sbyte y){ return ToSByte(x >> y); }
static short ashr16(short x, short y){ return ToSByte(x >> y); }
static int ashr32(int x, int y){ return x >> y; }
static long ashr64(long x, long y){ return x >> ToInt32(y); }

static sbyte sdiv8(sbyte x, sbyte y){ return ToSByte(x / y); }
static short sdiv16(short x, short y){ return ToSByte(x / y); }
static int sdiv32(int x, int y){ return x / y; }
static long sdiv64(long x, long y){ return x / y; }

static sbyte smod8(sbyte x, sbyte y){ return ToSByte(x % y); }
static short smod16(short x, short y){ return ToSByte(x % y); }
static int smod32(int x, int y){ return x % y; }
static long smod64(long x, long y){ return x % y; }

static sbyte udiv8(sbyte x, sbyte y){ return ToSByte(unsigned(x) / unsigned(y)); }
static short udiv16(short x, short y){ return ToInt16(unsigned(x) / unsigned(y)); }
static int udiv32(int x, int y){ return ToInt32(unsigned(x) / unsigned(y)); }
static long udiv64(long x, long y){ return ToInt64(unsigned(x) / unsigned(y)); }

static sbyte umod8(sbyte x, sbyte y){ return ToSByte(unsigned(x) % unsigned(y)); }
static short umod16(short x, short y){ return ToInt16(unsigned(x) % unsigned(y)); }
static int umod32(int x, int y){ return ToInt32(unsigned(x) % unsigned(y)); }
static long umod64(long x, long y){ return ToInt64(unsigned(x) % unsigned(y)); }

static sbyte squot8(sbyte x, sbyte y){ return ToSByte(ToSingle(x) / ToSingle(y)); }
static short squot16(short x, short y){ return ToInt16(ToSingle(x) / ToSingle(y)); }
static int squot32(int x, int y){ return ToInt32(ToSingle(x) / ToSingle(y)); }
static long squot64(long x, long y){ return ToInt64(ToSingle(x) / ToSingle(y)); }

// static Maybe change srem, it calls np.fmod originally so i dont know
static sbyte srem8(sbyte x, sbyte y){ return sdiv8(x,y);}
static short srem16(short x, short y){ return sdiv16(x,y);}
static int srem32(int x, int y){ return sdiv32(x,y);}
static long srem64(long x, long y){ return sdiv64(x,y);}

static sbyte smin8(sbyte x, sbyte y){ return Min(x,y);}
static short smin16(short x, short y){ return Min(x,y);}
static int smin32(int x, int y){ return Min(x,y);}
static long smin64(long x, long y){ return Min(x,y);}

static sbyte smax8(sbyte x, sbyte y){ return Max(x,y);}
static short smax16(short x, short y){ return Max(x,y);}
static int smax32(int x, int y){ return Max(x,y);}
static long smax64(long x, long y){ return Max(x,y);}

static sbyte umin8(sbyte x, sbyte y){ return signed(Min(unsigned(x),unsigned(y)));}
static short umin16(short x, short y){ return signed(Min(unsigned(x),unsigned(y)));}
static int umin32(int x, int y){ return signed(Min(unsigned(x),unsigned(y)));}
static long umin64(long x, long y){ return signed(Min(unsigned(x),unsigned(y)));}

static sbyte umax8(sbyte x, sbyte y){ return signed(Max(unsigned(x),unsigned(y)));}
static short umax16(short x, short y){ return signed(Max(unsigned(x),unsigned(y)));}
static int umax32(int x, int y){ return signed(Max(unsigned(x),unsigned(y)));}
static long umax64(long x, long y){ return signed(Max(unsigned(x),unsigned(y)));}

static float fmin32(float x, float y){ return Min(x,y);}
static double fmin64(double x, double y){ return Min(x,y);}
static float fmax32(float x, float y){ return Max(x,y);}
static double fmax64(double x, double y){ return Max(x,y);}

static sbyte pow8(sbyte x, sbyte y){ return ToSByte(Pow(x,y));}
static short pow16(short x, short y){ return ToInt16(Pow(x,y));}
static int pow32(int x, int y){ return ToInt32(Pow(x,y));}
static long pow64(long x, long y){ return ToInt64(Pow(x,y));}
static float fpow32(float x, float y){ return ToSingle(Pow(x,y));}
static double fpow64(double x, double y){ return ToDouble(Pow(x,y));}

static bool sle8(sbyte x, sbyte y){ return x <= y ;}
static bool sle16(short x, short y){ return x <= y ;}
static bool sle32(int x, int y){ return x <= y ;}
static bool sle64(long x, long y){ return x <= y ;}

static bool slt8(sbyte x, sbyte y){ return x < y ;}
static bool slt16(short x, short y){ return x < y ;}
static bool slt32(int x, int y){ return x < y ;}
static bool slt64(long x, long y){ return x < y ;}

static bool ule8(sbyte x, sbyte y){ return unsigned(x) <= unsigned(y) ;}
static bool ule16(short x, short y){ return unsigned(x) <= unsigned(y) ;}
static bool ule32(int x, int y){ return unsigned(x) <= unsigned(y) ;}
static bool ule64(long x, long y){ return unsigned(x) <= unsigned(y) ;}

static bool ult8(sbyte x, sbyte y){ return unsigned(x) < unsigned(y) ;}
static bool ult16(short x, short y){ return unsigned(x) < unsigned(y) ;}
static bool ult32(int x, int y){ return unsigned(x) < unsigned(y) ;}
static bool ult64(long x, long y){ return unsigned(x) < unsigned(y) ;}

static sbyte lshr8(sbyte x, sbyte y){ return ToSByte(ToByte(x) >> ToByte(y));}
static short lshr16(short x, short y){ return ToInt16(ToUInt16(x) >> ToUInt16(y));}
static int lshr32(int x, int y){ return ToInt32(ToUInt32(x) >> ToInt32(y));}
static long lshr64(long x, long y){ return ToInt64(ToUInt64(x) >> ToInt32(y));}

static sbyte sext_i8_i8(sbyte x){return ToSByte(x);}
static sbyte sext_i16_i8(short x){return ToSByte(x);}
static sbyte sext_i32_i8(int x){return ToSByte(x);}
static sbyte sext_i64_i8(long x){return ToSByte(x);}

static short sext_i8_i16(sbyte x){return ToInt16(x);}
static short sext_i16_i16(short x){return ToInt16(x);}
static short sext_i32_i16(int x){return ToInt16(x);}
static short sext_i64_i16(long x){return ToInt16(x);}

static int sext_i8_i32(sbyte x){return ToInt32(x);}
static int sext_i16_i32(short x){return ToInt32(x);}
static int sext_i32_i32(int x){return ToInt32(x);}
static int sext_i64_i32(long x){return ToInt32(x);}

static long sext_i8_i64(sbyte x){return ToInt64(x);}
static long sext_i16_i64(short x){return ToInt64(x);}
static long sext_i32_i64(int x){return ToInt64(x);}
static long sext_i64_i64(long x){return ToInt64(x);}


static sbyte zext_i8_i8(sbyte x){return ToSByte(ToByte(x));}
static sbyte zext_i16_i8(short x){return ToSByte(ToByte(x));}
static sbyte zext_i32_i8(int x){return ToSByte(ToByte(x));}
static sbyte zext_i64_i8(long x){return ToSByte(ToByte(x));}

static short zext_i8_i16(sbyte x){return ToInt16(ToUInt16(x));}
static short zext_i16_i16(short x){return ToInt16(ToUInt16(x));}
static short zext_i32_i16(int x){return ToInt16(ToUInt16(x));}
static short zext_i64_i16(long x){return ToInt16(ToUInt16(x));}

static int zext_i8_i32(sbyte x){return ToInt32(ToUInt32(x));}
static int zext_i16_i32(short x){return ToInt32(ToUInt32(x));}
static int zext_i32_i32(int x){return ToInt32(ToUInt32(x));}
static int zext_i64_i32(long x){return ToInt32(ToUInt32(x));}

static long zext_i8_i64(sbyte x){return ToInt64(ToUInt64(x));}
static long zext_i16_i64(short x){return ToInt64(ToUInt64(x));}
static long zext_i32_i64(int x){return ToInt64(ToUInt64(x));}
static long zext_i64_i64(long x){return ToInt64(ToUInt64(x));}

static int ssignum8(sbyte x){return Sign(x);}
static int ssignum16(short x){return Sign(x);}
static int ssignum32(int x){return Sign(x);}
static int ssignum64(long x){return Sign(x);}

static int usignum8(byte x){return x < 0 ? ssignum8(ToSByte(-x)) : ssignum8(ToSByte(x));}
static int usignum16(ushort x){return x < 0 ? ssignum16(ToInt16(-x)) : ssignum16(ToInt16(x));}
static int usignum32(uint x){return x < 0 ? ssignum32(ToInt32(-x)) : ssignum32(ToInt32(x));}
static int usignum64(ulong x){return x < 0 ? ssignum64(ToInt64(0-x)) : ssignum64(ToInt64(x));}

static float sitofp_i8_f32(sbyte x){return ToSingle(x);}
static float sitofp_i16_f32(short x){return ToSingle(x);}
static float sitofp_i32_f32(int x){return ToSingle(x);}
static float sitofp_i64_f32(long x){return ToSingle(x);}

static double sitofp_i8_f64(sbyte x){return ToDouble(x);}
static double sitofp_i16_f64(short x){return ToDouble(x);}
static double sitofp_i32_f64(int x){return ToDouble(x);}
static double sitofp_i64_f64(long x){return ToDouble(x);}


static float uitofp_i8_f32(sbyte x){return ToSingle(unsigned(x));}
static float uitofp_i16_f32(short x){return ToSingle(unsigned(x));}
static float uitofp_i32_f32(int x){return ToSingle(unsigned(x));}
static float uitofp_i64_f32(long x){return ToSingle(unsigned(x));}

static double uitofp_i8_f64(sbyte x){return ToDouble(unsigned(x));}
static double uitofp_i16_f64(short x){return ToDouble(unsigned(x));}
static double uitofp_i32_f64(int x){return ToDouble(unsigned(x));}
static double uitofp_i64_f64(long x){return ToDouble(unsigned(x));}

static byte fptoui_f32_i8(float x){return ToByte(Truncate(x));}
static byte fptoui_f64_i8(double x){return ToByte(Truncate(x));}
static sbyte fptosi_f32_i8(float x){return ToSByte(Truncate(x));}
static sbyte fptosi_f64_i8(double x){return ToSByte(Truncate(x));}

static ushort fptoui_f32_i16(float x){return ToUInt16(Truncate(x));}
static ushort fptoui_f64_i16(double x){return ToUInt16(Truncate(x));}
static short fptosi_f32_i16(float x){return ToInt16(Truncate(x));}
static short fptosi_f64_i16(double x){return ToInt16(Truncate(x));}

static uint fptoui_f32_i32(float x){return ToUInt32(Truncate(x));}
static uint fptoui_f64_i32(double x){return ToUInt32(Truncate(x));}
static int fptosi_f32_i32(float x){return ToInt32(Truncate(x));}
static int fptosi_f64_i32(double x){return ToInt32(Truncate(x));}

static ulong fptoui_f32_i64(float x){return ToUInt64(Truncate(x));}
static ulong fptoui_f64_i64(double x){return ToUInt64(Truncate(x));}
static long fptosi_f32_i64(float x){return ToInt64(Truncate(x));}
static long fptosi_f64_i64(double x){return ToInt64(Truncate(x));}

static double fpconv_f32_f64(float x){return ToDouble(x);}
static float fpconv_f64_f32(double x){return ToSingle(x);}

static double futhark_log64(double x){return Log(x);}
static double futhark_sqrt64(double x){return Sqrt(x);}
static double futhark_exp64(double x){return Exp(x);}
static double futhark_cos64(double x){return Cos(x);}
static double futhark_sin64(double x){return Sin(x);}
static double futhark_tan64(double x){return Tan(x);}
static double futhark_acos64(double x){return Acos(x);}
static double futhark_asin64(double x){return Asin(x);}
static double futhark_atan64(double x){return Atan(x);}
static double futhark_atan2_64(double x, double y){return Atan2(x, y);}
static bool futhark_isnan64(double x){return double.IsNaN(x);}
static bool futhark_isinf64(double x){return double.IsInfinity(x);}
static long futhark_to_bits64(double x){return BitConverter.ToInt64(BitConverter.GetBytes(x),0);}
static double futhark_from_bits64(long x){return BitConverter.ToDouble(BitConverter.GetBytes(x),0);}

static float futhark_log32(float x){return (float) Log(x);}
static float futhark_sqrt32(float x){return (float) Sqrt(x);}
static float futhark_exp32(float x){return (float) Exp(x);}
static float futhark_cos32(float x){return (float) Cos(x);}
static float futhark_sin32(float x){return (float) Sin(x);}
static float futhark_tan32(float x){return (float) Tan(x);}
static float futhark_acos32(float x){return (float) Acos(x);}
static float futhark_asin32(float x){return (float) Asin(x);}
static float futhark_atan32(float x){return (float) Atan(x);}
static float futhark_atan2_32(float x, float y){return (float) Atan2(x, y);}
static bool futhark_isnan32(float x){return float.IsNaN(x);}
static bool futhark_isinf32(float x){return float.IsInfinity(x);}
static int futhark_to_bits32(float x){return BitConverter.ToInt32(BitConverter.GetBytes(x), 0);}
static float futhark_from_bits32(int x){return BitConverter.ToSingle(BitConverter.GetBytes(x), 0);}

