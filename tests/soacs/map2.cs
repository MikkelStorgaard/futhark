#define DEBUG
using System;
using System.Diagnostics;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using static System.ValueTuple;
using static System.Convert;
using static System.Math;
using Mono.Options;
using Cloo;
using Cloo.Bindings;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
class FutharkInternal
{
    public FutharkInternal()
    {
        
    }
    // Libraries below are imported in SequentialCSharp.hs
    // using System;
    // using static System.Convert;
    // using static System.Math;
    
    // Scalar functions.
    static sbyte signed(byte x){ return Convert.ToSByte(x);}
    static short signed(ushort x){ return Convert.ToInt16(x);}
    static int signed(uint x){ return Convert.ToInt32(x);}
    static long signed(ulong x){ return Convert.ToInt64(x);}
    
    static byte unsigned(sbyte x){ return Convert.ToByte(x);}
    static ushort unsigned(short x){ return Convert.ToUInt16(x);}
    static uint unsigned(int x){ return Convert.ToUInt32(x);}
    static ulong unsigned(long x){ return Convert.ToUInt64(x);}
    
    static sbyte shl8(sbyte x, sbyte y){ return Convert.ToSByte(x << y); }
    static short shl16(short x, short y){ return Convert.ToInt16(x << y); }
    static int shl32(int x, int y){ return x << y; }
    static long shl64(long x, long y){ return x << Convert.ToInt32(y); }
    
    static sbyte ashr8(sbyte x, sbyte y){ return Convert.ToSByte(x >> y); }
    static short ashr16(short x, short y){ return Convert.ToSByte(x >> y); }
    static int ashr32(int x, int y){ return x >> y; }
    static long ashr64(long x, long y){ return x >> Convert.ToInt32(y); }
    
    static sbyte sdiv8(sbyte x, sbyte y){ return Convert.ToSByte(x / y); }
    static short sdiv16(short x, short y){ return Convert.ToSByte(x / y); }
    static int sdiv32(int x, int y){ return x / y; }
    static long sdiv64(long x, long y){ return x / y; }
    
    static sbyte smod8(sbyte x, sbyte y){ return Convert.ToSByte(x % y); }
    static short smod16(short x, short y){ return Convert.ToSByte(x % y); }
    static int smod32(int x, int y){ return x % y; }
    static long smod64(long x, long y){ return x % y; }
    
    static sbyte udiv8(sbyte x, sbyte y){ return Convert.ToSByte(unsigned(x) / unsigned(y)); }
    static short udiv16(short x, short y){ return Convert.ToInt16(unsigned(x) / unsigned(y)); }
    static int udiv32(int x, int y){ return Convert.ToInt32(unsigned(x) / unsigned(y)); }
    static long udiv64(long x, long y){ return Convert.ToInt64(unsigned(x) / unsigned(y)); }
    
    static sbyte umod8(sbyte x, sbyte y){ return Convert.ToSByte(unsigned(x) % unsigned(y)); }
    static short umod16(short x, short y){ return Convert.ToInt16(unsigned(x) % unsigned(y)); }
    static int umod32(int x, int y){ return Convert.ToInt32(unsigned(x) % unsigned(y)); }
    static long umod64(long x, long y){ return Convert.ToInt64(unsigned(x) % unsigned(y)); }
    
    static sbyte squot8(sbyte x, sbyte y){ return Convert.ToSByte(ToSingle(x) / ToSingle(y)); }
    static short squot16(short x, short y){ return Convert.ToInt16(ToSingle(x) / ToSingle(y)); }
    static int squot32(int x, int y){ return Convert.ToInt32(ToSingle(x) / ToSingle(y)); }
    static long squot64(long x, long y){ return Convert.ToInt64(ToSingle(x) / ToSingle(y)); }
    
    // static Maybe change srem, it calls np.fmod originally so i dont know
    static sbyte srem8(sbyte x, sbyte y){ return sdiv8(x,y);}
    static short srem16(short x, short y){ return sdiv16(x,y);}
    static int srem32(int x, int y){ return sdiv32(x,y);}
    static long srem64(long x, long y){ return sdiv64(x,y);}
    
    static sbyte smin8(sbyte x, sbyte y){ return Math.Min(x,y);}
    static short smin16(short x, short y){ return Math.Min(x,y);}
    static int smin32(int x, int y){ return Math.Min(x,y);}
    static long smin64(long x, long y){ return Math.Min(x,y);}
    
    static sbyte smax8(sbyte x, sbyte y){ return Math.Max(x,y);}
    static short smax16(short x, short y){ return Math.Max(x,y);}
    static int smax32(int x, int y){ return Math.Max(x,y);}
    static long smax64(long x, long y){ return Math.Max(x,y);}
    
    static sbyte umin8(sbyte x, sbyte y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
    static short umin16(short x, short y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
    static int umin32(int x, int y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
    static long umin64(long x, long y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
    
    static sbyte umax8(sbyte x, sbyte y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
    static short umax16(short x, short y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
    static int umax32(int x, int y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
    static long umax64(long x, long y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
    
    static float fmin32(float x, float y){ return Math.Min(x,y);}
    static double fmin64(double x, double y){ return Math.Min(x,y);}
    static float fmax32(float x, float y){ return Math.Max(x,y);}
    static double fmax64(double x, double y){ return Math.Max(x,y);}
    
    static sbyte pow8(sbyte x, sbyte y){ return Convert.ToSByte(Math.Pow(x,y));}
    static short pow16(short x, short y){ return Convert.ToInt16(Math.Pow(x,y));}
    static int pow32(int x, int y){ return Convert.ToInt32(Math.Pow(x,y));}
    static long pow64(long x, long y){ return Convert.ToInt64(Math.Pow(x,y));}
    static float fpow32(float x, float y){ return Convert.ToSingle(Math.Pow(x,y));}
    static double fpow64(double x, double y){ return Convert.ToDouble(Math.Pow(x,y));}
    
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
    
    static sbyte lshr8(sbyte x, sbyte y){ return Convert.ToSByte(Convert.ToByte(x) >> Convert.ToByte(y));}
    static short lshr16(short x, short y){ return Convert.ToInt16(Convert.ToUInt16(x) >> Convert.ToUInt16(y));}
    static int lshr32(int x, int y){ return Convert.ToInt32(Convert.ToUInt32(x) >> Convert.ToInt32(y));}
    static long lshr64(long x, long y){ return Convert.ToInt64(Convert.ToUInt64(x) >> Convert.ToInt32(y));}
    
    static sbyte sext_i8_i8(sbyte x){return Convert.ToSByte(x);}
    static sbyte sext_i16_i8(short x){return Convert.ToSByte(x);}
    static sbyte sext_i32_i8(int x){return Convert.ToSByte(x);}
    static sbyte sext_i64_i8(long x){return Convert.ToSByte(x);}
    
    static short sext_i8_i16(sbyte x){return Convert.ToInt16(x);}
    static short sext_i16_i16(short x){return Convert.ToInt16(x);}
    static short sext_i32_i16(int x){return Convert.ToInt16(x);}
    static short sext_i64_i16(long x){return Convert.ToInt16(x);}
    
    static int sext_i8_i32(sbyte x){return Convert.ToInt32(x);}
    static int sext_i16_i32(short x){return Convert.ToInt32(x);}
    static int sext_i32_i32(int x){return Convert.ToInt32(x);}
    static int sext_i64_i32(long x){return Convert.ToInt32(x);}
    
    static long sext_i8_i64(sbyte x){return Convert.ToInt64(x);}
    static long sext_i16_i64(short x){return Convert.ToInt64(x);}
    static long sext_i32_i64(int x){return Convert.ToInt64(x);}
    static long sext_i64_i64(long x){return Convert.ToInt64(x);}
    
    
    static sbyte zext_i8_i8(sbyte x){return Convert.ToSByte(Convert.ToByte(x));}
    static sbyte zext_i16_i8(short x){return Convert.ToSByte(Convert.ToByte(x));}
    static sbyte zext_i32_i8(int x){return Convert.ToSByte(Convert.ToByte(x));}
    static sbyte zext_i64_i8(long x){return Convert.ToSByte(Convert.ToByte(x));}
    
    static short zext_i8_i16(sbyte x){return Convert.ToInt16(Convert.ToUInt16(x));}
    static short zext_i16_i16(short x){return Convert.ToInt16(Convert.ToUInt16(x));}
    static short zext_i32_i16(int x){return Convert.ToInt16(Convert.ToUInt16(x));}
    static short zext_i64_i16(long x){return Convert.ToInt16(Convert.ToUInt16(x));}
    
    static int zext_i8_i32(sbyte x){return Convert.ToInt32(Convert.ToUInt32(x));}
    static int zext_i16_i32(short x){return Convert.ToInt32(Convert.ToUInt32(x));}
    static int zext_i32_i32(int x){return Convert.ToInt32(Convert.ToUInt32(x));}
    static int zext_i64_i32(long x){return Convert.ToInt32(Convert.ToUInt32(x));}
    
    static long zext_i8_i64(sbyte x){return Convert.ToInt64(Convert.ToUInt64(x));}
    static long zext_i16_i64(short x){return Convert.ToInt64(Convert.ToUInt64(x));}
    static long zext_i32_i64(int x){return Convert.ToInt64(Convert.ToUInt64(x));}
    static long zext_i64_i64(long x){return Convert.ToInt64(Convert.ToUInt64(x));}
    
    static int ssignum8(sbyte x){return Math.Sign(x);}
    static int ssignum16(short x){return Math.Sign(x);}
    static int ssignum32(int x){return Math.Sign(x);}
    static int ssignum64(long x){return Math.Sign(x);}
    
    static int usignum8(byte x){return x < 0 ? ssignum8(Convert.ToSByte(-x)) : ssignum8(Convert.ToSByte(x));}
    static int usignum16(ushort x){return x < 0 ? ssignum16(Convert.ToInt16(-x)) : ssignum16(Convert.ToInt16(x));}
    static int usignum32(uint x){return x < 0 ? ssignum32(Convert.ToInt32(-x)) : ssignum32(Convert.ToInt32(x));}
    static int usignum64(ulong x){return x < 0 ? ssignum64(Convert.ToInt64(0-x)) : ssignum64(Convert.ToInt64(x));}
    
    static int ssignum(sbyte x){return Math.Sign(x);}
    static int ssignum(short x){return Math.Sign(x);}
    static int ssignum(int x){return Math.Sign(x);}
    static int ssignum(long x){return Math.Sign(x);}
    
    static int usignum(byte x){return x < 0 ? ssignum8(Convert.ToSByte(-x)) : ssignum8(Convert.ToSByte(x));}
    static int usignum(ushort x){return x < 0 ? ssignum16(Convert.ToInt16(-x)) : ssignum16(Convert.ToInt16(x));}
    static int usignum(uint x){return x < 0 ? ssignum32(Convert.ToInt32(-x)) : ssignum32(Convert.ToInt32(x));}
    static int usignum(ulong x){return x < 0 ? ssignum64(Convert.ToInt64(0-x)) : ssignum64(Convert.ToInt64(x));}
    
    static float sitofp_i8_f32(sbyte x){return Convert.ToSingle(x);}
    static float sitofp_i16_f32(short x){return Convert.ToSingle(x);}
    static float sitofp_i32_f32(int x){return Convert.ToSingle(x);}
    static float sitofp_i64_f32(long x){return Convert.ToSingle(x);}
    
    static double sitofp_i8_f64(sbyte x){return Convert.ToDouble(x);}
    static double sitofp_i16_f64(short x){return Convert.ToDouble(x);}
    static double sitofp_i32_f64(int x){return Convert.ToDouble(x);}
    static double sitofp_i64_f64(long x){return Convert.ToDouble(x);}
    
    
    static float uitofp_i8_f32(sbyte x){return Convert.ToSingle(unsigned(x));}
    static float uitofp_i16_f32(short x){return Convert.ToSingle(unsigned(x));}
    static float uitofp_i32_f32(int x){return Convert.ToSingle(unsigned(x));}
    static float uitofp_i64_f32(long x){return Convert.ToSingle(unsigned(x));}
    
    static double uitofp_i8_f64(sbyte x){return Convert.ToDouble(unsigned(x));}
    static double uitofp_i16_f64(short x){return Convert.ToDouble(unsigned(x));}
    static double uitofp_i32_f64(int x){return Convert.ToDouble(unsigned(x));}
    static double uitofp_i64_f64(long x){return Convert.ToDouble(unsigned(x));}
    
    static byte fptoui_f32_i8(float x){return Convert.ToByte(Math.Truncate(x));}
    static byte fptoui_f64_i8(double x){return Convert.ToByte(Math.Truncate(x));}
    static sbyte fptosi_f32_i8(float x){return Convert.ToSByte(Math.Truncate(x));}
    static sbyte fptosi_f64_i8(double x){return Convert.ToSByte(Math.Truncate(x));}
    
    static ushort fptoui_f32_i16(float x){return Convert.ToUInt16(Math.Truncate(x));}
    static ushort fptoui_f64_i16(double x){return Convert.ToUInt16(Math.Truncate(x));}
    static short fptosi_f32_i16(float x){return Convert.ToInt16(Math.Truncate(x));}
    static short fptosi_f64_i16(double x){return Convert.ToInt16(Math.Truncate(x));}
    
    static uint fptoui_f32_i32(float x){return Convert.ToUInt32(Math.Truncate(x));}
    static uint fptoui_f64_i32(double x){return Convert.ToUInt32(Math.Truncate(x));}
    static int fptosi_f32_i32(float x){return Convert.ToInt32(Math.Truncate(x));}
    static int fptosi_f64_i32(double x){return Convert.ToInt32(Math.Truncate(x));}
    
    static ulong fptoui_f32_i64(float x){return Convert.ToUInt64(Math.Truncate(x));}
    static ulong fptoui_f64_i64(double x){return Convert.ToUInt64(Math.Truncate(x));}
    static long fptosi_f32_i64(float x){return Convert.ToInt64(Math.Truncate(x));}
    static long fptosi_f64_i64(double x){return Convert.ToInt64(Math.Truncate(x));}
    
    static double fpconv_f32_f64(float x){return Convert.ToDouble(x);}
    static float fpconv_f64_f32(double x){return Convert.ToSingle(x);}
    
    static double futhark_log64(double x){return Math.Log(x);}
    static double futhark_sqrt64(double x){return Math.Sqrt(x);}
    static double futhark_exp64(double x){return Math.Exp(x);}
    static double futhark_cos64(double x){return Math.Cos(x);}
    static double futhark_sin64(double x){return Math.Sin(x);}
    static double futhark_tan64(double x){return Math.Tan(x);}
    static double futhark_acos64(double x){return Math.Acos(x);}
    static double futhark_asin64(double x){return Math.Asin(x);}
    static double futhark_atan64(double x){return Math.Atan(x);}
    static double futhark_atan2_64(double x, double y){return Math.Atan2(x, y);}
    static bool futhark_isnan64(double x){return double.IsNaN(x);}
    static bool futhark_isinf64(double x){return double.IsInfinity(x);}
    static long futhark_to_bits64(double x){return BitConverter.ToInt64(BitConverter.GetBytes(x),0);}
    static double futhark_from_bits64(long x){return BitConverter.ToDouble(BitConverter.GetBytes(x),0);}
    
    static float futhark_log32(float x){return (float) Math.Log(x);}
    static float futhark_sqrt32(float x){return (float) Math.Sqrt(x);}
    static float futhark_exp32(float x){return (float) Math.Exp(x);}
    static float futhark_cos32(float x){return (float) Math.Cos(x);}
    static float futhark_sin32(float x){return (float) Math.Sin(x);}
    static float futhark_tan32(float x){return (float) Math.Tan(x);}
    static float futhark_acos32(float x){return (float) Math.Acos(x);}
    static float futhark_asin32(float x){return (float) Math.Asin(x);}
    static float futhark_atan32(float x){return (float) Math.Atan(x);}
    static float futhark_atan2_32(float x, float y){return (float) Math.Atan2(x, y);}
    static bool futhark_isnan32(float x){return float.IsNaN(x);}
    static bool futhark_isinf32(float x){return float.IsInfinity(x);}
    static int futhark_to_bits32(float x){return BitConverter.ToInt32(BitConverter.GetBytes(x), 0);}
    static float futhark_from_bits32(int x){return BitConverter.ToSingle(BitConverter.GetBytes(x), 0);}
    
    public class FlatArray<T>
    {
        public long[] shape;
        public T[] array;
        public FlatArray(T[] data_array, long[] shape_array)
        {
            shape = shape_array;
            array = data_array;
        }
    
        public FlatArray(T[] data_array)
        {
            shape = new long[] {data_array.Length};
            array = data_array;
        }
    
        private long getIdx(int[] idxs)
        {
            long idx = 0;
            for (int i = 0; i<idxs.Length; i++)
            {
                idx += shape[i] * idxs[i];
            }
            return idx;
    
        }
        public T this[params int[] indexes]
        {
            get
            {
                Debug.Assert(indexes.Length == shape.Length);
                return array[getIdx(indexes)];
            }
    
            set
            {
                Debug.Assert(indexes.Length == shape.Length);
                array[getIdx(indexes)] = value;
            }
        }
    
        public IEnumerator GetEnumerator()
        {
            foreach (T val in array)
            {
                yield return val;
            }
        }
    }
    
    public class Opaque{
        object desc;
        object data;
        public Opaque(string str, object payload)
        {
            this.desc = str;
            this.data = payload;
        }
    
        public override string ToString()
        {
            return string.Format("<opaque Futhark value of type {}>", desc);
        }
    }
    
    byte[] allocateMem(sbyte size)
    {
        return new byte[size];
    }
    byte[] allocateMem(short size)
    {
        return new byte[size];
    }
    byte[] allocateMem(int size)
    {
        return new byte[size];
    }
    byte[] allocateMem(long size)
    {
        return new byte[size];
    }
    byte[] allocateMem(byte size)
    {
        return new byte[size];
    }
    byte[] allocateMem(ushort size)
    {
        return new byte[size];
    }
    byte[] allocateMem(uint size)
    {
        return new byte[size];
    }
    byte[] allocateMem(ulong size)
    {
        return new byte[size];
    }
    
    FlatArray<byte> createArray_byte(byte[] bytes, long[] shape)
    {
        var byteArray = new byte[bytes.Length / sizeof(byte)];
        Buffer.BlockCopy(bytes, 0, byteArray, 0, bytes.Length);
        return new FlatArray<byte>(byteArray, shape);
    }
    FlatArray<ushort> createArray_ushort(byte[] bytes, long[] shape)
    {
        var ushortArray = new ushort[bytes.Length / sizeof(ushort)];
        Buffer.BlockCopy(bytes, 0, ushortArray, 0, bytes.Length);
        return new FlatArray<ushort>(ushortArray, shape);
    }
    
    FlatArray<uint> createArray_uint(byte[] bytes, long[] shape)
    {
        var uintArray = new uint[bytes.Length / sizeof(uint)];
        Buffer.BlockCopy(bytes, 0, uintArray, 0, bytes.Length);
        return new FlatArray<uint>(uintArray, shape);
    }
    
    FlatArray<ulong> createArray_ulong(byte[] bytes, long[] shape)
    {
        var ulongArray = new ulong[bytes.Length / sizeof(ulong)];
        Buffer.BlockCopy(bytes, 0, ulongArray, 0, bytes.Length);
        return new FlatArray<ulong>(ulongArray, shape);
    }
    
    FlatArray<sbyte> createArray_sbyte(byte[] bytes, long[] shape)
    {
        var sbyteArray = new sbyte[bytes.Length / sizeof(sbyte)];
        Buffer.BlockCopy(bytes, 0, sbyteArray, 0, bytes.Length);
        return new FlatArray<sbyte>(sbyteArray, shape);
    }
    
    FlatArray<short> createArray_short(byte[] bytes, long[] shape)
    {
        var shortArray = new short[bytes.Length / sizeof(short)];
        Buffer.BlockCopy(bytes, 0, shortArray, 0, bytes.Length);
        return new FlatArray<short>(shortArray, shape);
    }
    
    FlatArray<int> createArray_int(byte[] bytes, long[] shape)
    {
        var intArray = new int[bytes.Length / sizeof(int)];
        Buffer.BlockCopy(bytes, 0, intArray, 0, bytes.Length);
        return new FlatArray<int>(intArray, shape);
    }
    
    FlatArray<long> createArray_long(byte[] bytes, long[] shape)
    {
        var longArray = new long[bytes.Length / sizeof(long)];
        Buffer.BlockCopy(bytes, 0, longArray, 0, bytes.Length);
        return new FlatArray<long>(longArray, shape);
    }
    
    FlatArray<float> createArray_float(byte[] bytes, long[] shape)
    {
        var floatArray = new float[bytes.Length / sizeof(float)];
        Buffer.BlockCopy(bytes, 0, floatArray, 0, bytes.Length);
        return new FlatArray<float>(floatArray, shape);
    }
    
    FlatArray<double> createArray_double(byte[] bytes, long[] shape)
    {
        var doubleArray = new double[bytes.Length / sizeof(double)];
        Buffer.BlockCopy(bytes, 0, doubleArray, 0, bytes.Length);
        return new FlatArray<double>(doubleArray, shape);
    }
    
    FlatArray<bool> createArray_bool(byte[] bytes, long[] shape)
    {
        var boolArray = new bool[bytes.Length / sizeof(bool)];
        Buffer.BlockCopy(bytes, 0, boolArray, 0, bytes.Length);
        return new FlatArray<bool>(boolArray, shape);
    }
    
    
    /*
      # Helper functions dealing with memory blocks.
    def allocateMem(size):
      return ct.cast((ct.c_byte * max(0,size))(), ct.POINTER(ct.c_byte))
    
    # Copy an array if its is not-None.  This is important for treating
    # Numpy arrays as flat memory, but has some overhead.
    def normaliseArray(x):
      if (x.base is x) or (x.base is None):
        return x
      else:
        return x.copy()
    
    def unwrapArray(x):
      return normaliseArray(x).ctypes.data_as(ct.POINTER(ct.c_byte))
    
    def createArray(x, dim):
      return np.ctypeslib.as_array(x, shape=dim)
    
    # An opaque Futhark value.
    class opaque(object):
      def __init__(self, desc, *payload):
        self.data = payload
        self.desc = desc
    
      def __repr__(self):
        return "<opaque Futhark value of type {}>".format(self.desc)
    
    */
    
    
    
    byte[] unwrapArray(Array src, int obj_size)
    {
        var bytes = new byte[src.Length * obj_size];
        Buffer.BlockCopy(src, 0, bytes, 0, bytes.Length);
        return bytes;
    }
    
    T indexArray<T>(byte[] src, int offset, Func<byte[],int, T> converter)
    {
        return converter(src, offset);
    }
    
    void writeScalarArray(byte[] dest, int offset, sbyte value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, byte value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, short value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, ushort value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, int value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, uint value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, long value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, ulong value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, float value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, double value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    void writeScalarArray(byte[] dest, int offset, bool value)
    {
        var asBytes = BitConverter.GetBytes(value);
        Buffer.BlockCopy(asBytes, 0, dest, offset, asBytes.Length);
    }
    public class TypeError : Exception
    {
        public TypeError(){}
        public TypeError(string message):base(message){}
        public TypeError(string message, Exception inner):base(message, inner){}
    }
        internal Stack<char> LookaheadBuffer = new Stack<char>();
        void ResetLookahead(){LookaheadBuffer.Clear();}
    
        char? GetChar(Stream f)
        {
            char c;
            if (LookaheadBuffer.Count == 0)
            {
                c = (char) f.ReadByte();
            }
            else
            {
                c = LookaheadBuffer.Pop();
            }
    
            return c;
        }
    
        char[] GetChars(Stream f, int n)
        {
            return Enumerable.Range(0, n).Select(_ => GetChar(f).Value).ToArray();
        }
    
        void UngetChar(char c)
        {
            LookaheadBuffer.Push(c);
        }
    
        char PeekChar(Stream f)
        {
            var c = GetChar(f);
            UngetChar(c.Value);
            return c.Value;
        }
    
        void SkipSpaces(Stream f)
        {
            var c = GetChar(f);
            while (c.HasValue){
                if (char.IsWhiteSpace(c.Value))
                {
                    c = GetChar(f);
                }
                else if (c == '-')
                {
                    if (PeekChar(f) == '-')
                    {
                        while (c.Value != '\n')
                        {
                            c = GetChar(f);
                        }
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
        
            if (c.HasValue)
            {
                UngetChar(c.Value);
            }
        }
    
        bool ParseSpecificChar(Stream f, char c)
        {
            var got = GetChar(f);
            if (got.Value != c)
            {
                UngetChar(got.Value);
                throw new Exception("ValueError");
            }
            return true;
        }
    
        bool ParseSpecificString(Stream f, string str)
        {
            foreach (var c in str.ToCharArray())
            {
                ParseSpecificChar(f, c);
            }
    
            return true;
        }
    
    
        string Optional(Func<Stream, string> p, Stream f)
        {
            string res = null;
            try
            {
                res = p(f);
            }
            catch (Exception)
            {
            }
    
            return res;
        }
        
        bool Optional(Func<Stream, char, bool> p, Stream f, char c)
        {
            try
            {
                return p(f,c);
            }
            catch (Exception)
            {
            }
    
            return false;
        }
    
        bool OptionalSpecificString(Stream f, string s)
        {
            var c = PeekChar(f);
            if (c == s[0])
            {
                return ParseSpecificString(f, s);
            }
            return false;
        }
            
            
            List<string> sepBy(Func<Stream, string> p, Func<Stream, string> sep, Stream arg)
            {
                var elems = new List<string>();
                var x = Optional(p, arg);
                if (!string.IsNullOrWhiteSpace(x))
                {
                    elems.Add(x);
                    while (!string.IsNullOrWhiteSpace(Optional(sep, arg)))
                    {
                        var y = Optional(p, arg);
                        elems.Add(y);
                    } 
                }
                return elems;
            }
    
        string ParseHexInt(Stream f)
        {
            var s = "";
            var c = GetChar(f);
            while (c.HasValue)
            {
                if (Uri.IsHexDigit(c.Value))
                {
                    s += c.Value;
                    c = GetChar(f);
                }
                else if (c == '_')
                {
                    c = GetChar(f);
                }
                else
                {
                    UngetChar(c.Value);
                    break;
                }
            }
    
            return Convert.ToString(Convert.ToUInt32(s, 16));
        }
    
        string ParseInt(Stream f)
        {
            var s = "";
            var c = GetChar(f);
            if (c.Value == '0' && "xX".Contains(PeekChar(f)))
            {
                GetChar(f);
                s += ParseHexInt(f);
            }
            else
            {
                while (c.HasValue)
                {
                    if (char.IsDigit(c.Value))
                    {
                        s += c.Value;                    
                        c = GetChar(f);
                    }else if (c == '_')
                    {
                        c = GetChar(f);
                    }
                    else
                    {
                        UngetChar(c.Value);
                        break;
                    }
                }
                
            }
    
            if (s.Length == 0)
            {
                throw new Exception("ValueError");
            }
    
            return s;
        }
    
        string ParseIntSigned(Stream f)
        {
            var s = "";
            var c = GetChar(f);
            if (c.Value == '-' && char.IsDigit(PeekChar(f)))
            {
                return c + ParseInt(f);
            }
            else
            {
                if (c.Value != '+')
                {
                    UngetChar(c.Value);
                }
    
                return ParseInt(f);
            }
        }
    
        string ReadStrComma(Stream f)
        {
            SkipSpaces(f);
            ParseSpecificChar(f, ',');
            return ",";
        }
    
        int ReadStrInt(Stream f, string s)
        {
            SkipSpaces(f);
            var x = Convert.ToInt32(ParseIntSigned(f));
            OptionalSpecificString(f, s);
            return x;
        }
        
        int ReadStrUint(Stream f, string s)
        {
            SkipSpaces(f);
            var x = Convert.ToInt32(ParseInt(f));
            OptionalSpecificString(f, s);
            return x;
        }
    
        int ReadStrI8(Stream f){return ReadStrInt(f, "i8");}
        int ReadStrI16(Stream f){return ReadStrInt(f, "i16");}
        int ReadStrI32(Stream f){return ReadStrInt(f, "i32");}
        int ReadStrI64(Stream f){return ReadStrInt(f, "i64");}
        int ReadStrU8(Stream f){return ReadStrInt(f, "u8");}
        int ReadStrU16(Stream f){return ReadStrInt(f, "u16");}
        int ReadStrU32(Stream f){return ReadStrInt(f, "u32");}
        int ReadStrU64(Stream f){return ReadStrInt(f, "u64");}
    
        char ReadChar(Stream f)
        {
            SkipSpaces(f);
            ParseSpecificChar(f, '\'');
            var c = GetChar(f);
            ParseSpecificChar(f, '\'');
            return c.Value;
        }
    
        float ReadStrHexFloat(Stream f, char sign)
        {
            var int_part = ParseHexInt(f);
            ParseSpecificChar(f, '.');
            var frac_part = ParseHexInt(f);
            ParseSpecificChar(f, 'p');
            var exponent = ParseHexInt(f);
    
            var int_val = Convert.ToInt32(int_part, 16);
            var frac_val = Convert.ToSingle(Convert.ToInt32(frac_part, 16)) / Math.Pow(16, frac_part.Length);
            var exp_val = Convert.ToInt32(exponent);
    
            var total_val = (int_val + frac_val) * Math.Pow(2, exp_val);
            if (sign == '-')
            {
                total_val = -1 * total_val;
            }
    
            return Convert.ToSingle(total_val);
        }
    
        float ReadStrDecimal(Stream f)
        {
            SkipSpaces(f);
            var c = GetChar(f);
            char sign;
            if (c.Value == '-')
            {
                sign = '-';
            }
            else
            {
                UngetChar(c.Value);
                sign = '+';
            }
            
            // Check for hexadecimal float
            c = GetChar(f);
            if (c.Value == '0' && "xX".Contains(PeekChar(f)))
            {
                GetChar(f);
                return ReadStrHexFloat(f, sign);
            }
            else
            {
                UngetChar(c.Value);
            }
    
            var bef = Optional(ParseInt, f);
            var aft = "";
            if (string.IsNullOrEmpty(bef))
            {
                bef = "0";
                ParseSpecificChar(f, '.');
                aft = ParseInt(f);
            }else if (Optional(ParseSpecificChar, f, '.'))
            {
                aft = ParseInt(f);
            }
            else
            {
                aft = "0";
            }
    
            var expt = "";
            if (Optional(ParseSpecificChar, f, 'E') ||
                Optional(ParseSpecificChar, f, 'e'))
            {
                expt = ParseIntSigned(f);
            }
            else
            {
                expt = "0";
            }
    
            return Convert.ToSingle(sign + bef + "." + aft + "E" + expt);
        }
    
        float ReadStrF32(Stream f)
        {
            var x = ReadStrDecimal(f);
            OptionalSpecificString(f, "f32");
            return x;
        }
        
        float ReadStrF64(Stream f)
        {
            var x = ReadStrDecimal(f);
            OptionalSpecificString(f, "f64");
            return x;
        }
    
        bool ReadStrBool(Stream f)
        {
            SkipSpaces(f);
            if (PeekChar(f) == 't')
            {
                ParseSpecificString(f, "true");
                return true;
            }
    
            if (PeekChar(f) == 'f')
            {
                ParseSpecificString(f, "false");
                return false;
            }
    
            throw new Exception("ValueError");
        }
    
        sbyte read_i8(Stream f)
        {
            return (sbyte) ReadStrI8(f);
        }
        short read_i16(Stream f)
        {
            return (short) ReadStrI16(f);
        }
        int read_i32(Stream f)
        {
            return ReadStrI32(f);
        }
        long read_i64(Stream f)
        {
            return ReadStrI64(f);
        }
        
        byte read_u8(Stream f)
        {
            return (byte) ReadStrU8(f);
        }
        ushort read_u16(Stream f)
        {
            return (ushort) ReadStrU16(f);
        }
        uint read_u32(Stream f)
        {
            return (uint) ReadStrU32(f);
        }
        ulong read_u64(Stream f)
        {
            return (ulong) ReadStrU64(f);
        }
        
        bool read_bool(Stream f)
        {
            return ReadStrBool(f);
        }
        
        float read_f32(Stream f)
        {
            return ReadStrDecimal(f);
        }
        double read_f64(Stream f)
        {
            return ReadStrDecimal(f);
        }
    
    
        Stream getStream()
        {
            return Console.OpenStandardInput();
        }
    // Stub code for OpenCL setup.
    
    void OPENCL_SUCCEED(int return_code,
                            [CallerFilePath] string filePath = "",
                            [CallerLineNumber] int lineNumber = 0)
    {
        opencl_succeed(return_code, "", filePath, lineNumber);
    }
    
    void OPENCL_SUCCEED(ComputeErrorCode return_code,
                        [CallerFilePath] string filePath = "",
                        [CallerLineNumber] int lineNumber = 0)
    {
        opencl_succeed((int) return_code, "", filePath, lineNumber);
    }
    
    void OPENCL_SUCCEED(object return_code,
                        [CallerFilePath] string filePath = "",
                        [CallerLineNumber] int lineNumber = 0)
    {
        opencl_succeed((int) return_code, "", filePath, lineNumber);
    }
    
    struct opencl_config
    {
        public bool debugging;
        public int preferred_device_num;
        public string preferred_platform;
        public string preferred_device;
    
        public string dump_program_to;
        public string load_program_from;
    
        public int default_group_size;
        public int default_num_groups;
        public int default_tile_size;
        public int default_threshold;
        public int transpose_block_dim;
    
        public int num_sizes;
        public string[] size_names;
        public int[] size_values;
        public string[] size_classes;
    }
    
    void opencl_config_init(out opencl_config cfg,
                            int num_sizes,
                            string[] size_names,
                            int[] size_values,
                            string[] size_classes)
    {
        cfg.debugging = false;
        cfg.preferred_device_num = 0;
        cfg.preferred_platform = "";
        cfg.preferred_device = "";
        cfg.dump_program_to = null;
        cfg.load_program_from = null;
    
        cfg.default_group_size = 256;
        cfg.default_num_groups = 128;
        cfg.default_tile_size = 32;
        cfg.default_threshold = 32*1024;
        cfg.transpose_block_dim = 16;
    
        cfg.num_sizes = num_sizes;
        cfg.size_names = size_names;
        cfg.size_values = size_values;
        cfg.size_classes = size_classes;
    }
    
    struct opencl_context {
       public CLPlatformHandle platform;
       public CLDeviceHandle device;
       public CLContextHandle context;
       public CLCommandQueueHandle queue;
    
       public opencl_config cfg;
    
       public int max_group_size;
       public int max_num_groups;
       public int max_tile_size;
       public int max_threshold;
    
       public int lockstep_width;
    }
    
    struct opencl_device_option {
        public CLPlatformHandle platform;
        public CLDeviceHandle device;
        public ComputeDeviceTypes device_type;
        public string platform_name;
        public string device_name;
    };
    
    /* This function must be defined by the user.  It is invoked by
       setup_opencl() after the platform and device has been found, but
       before the program is loaded.  Its intended use is to tune
       constants based on the selected platform and device. */
    
    string opencl_error_string(int err)
    {
        switch ((ComputeErrorCode) err) {
            case ComputeErrorCode.Success:                                        return "Success!";
            case ComputeErrorCode.DeviceNotFound:                                 return "Device not found.";
            case ComputeErrorCode.DeviceNotAvailable:                             return "Device not available";
            case ComputeErrorCode.CompilerNotAvailable:                           return "Compiler not available";
            case ComputeErrorCode.MemoryObjectAllocationFailure:                  return "Memory object allocation failure";
            case ComputeErrorCode.OutOfResources:                                 return "Out of resources";
            case ComputeErrorCode.OutOfHostMemory:                                return "Out of host memory";
            case ComputeErrorCode.ProfilingInfoNotAvailable:                      return "Profiling information not available";
            case ComputeErrorCode.MemoryCopyOverlap:                              return "Memory copy overlap";
            case ComputeErrorCode.ImageFormatMismatch:                            return "Image format mismatch";
            case ComputeErrorCode.ImageFormatNotSupported:                        return "Image format not supported";
            case ComputeErrorCode.BuildProgramFailure:                            return "Program build failure";
            case ComputeErrorCode.MapFailure:                                     return "Map failure";
            case ComputeErrorCode.InvalidValue:                                   return "Invalid value";
            case ComputeErrorCode.InvalidDeviceType:                              return "Invalid device type";
            case ComputeErrorCode.InvalidPlatform:                                return "Invalid platform";
            case ComputeErrorCode.InvalidDevice:                                  return "Invalid device";
            case ComputeErrorCode.InvalidContext:                                 return "Invalid context";
            case ComputeErrorCode.InvalidCommandQueueFlags:                       return "Invalid queue properties";
            case ComputeErrorCode.InvalidCommandQueue:                            return "Invalid command queue";
            case ComputeErrorCode.InvalidHostPointer:                             return "Invalid host pointer";
            case ComputeErrorCode.InvalidMemoryObject:                            return "Invalid memory object";
            case ComputeErrorCode.InvalidImageFormatDescriptor:                   return "Invalid image format descriptor";
            case ComputeErrorCode.InvalidImageSize:                               return "Invalid image size";
            case ComputeErrorCode.InvalidSampler:                                 return "Invalid sampler";
            case ComputeErrorCode.InvalidBinary:                                  return "Invalid binary";
            case ComputeErrorCode.InvalidBuildOptions:                            return "Invalid build options";
            case ComputeErrorCode.InvalidProgram:                                 return "Invalid program";
            case ComputeErrorCode.InvalidProgramExecutable:                       return "Invalid program executable";
            case ComputeErrorCode.InvalidKernelName:                              return "Invalid kernel name";
            case ComputeErrorCode.InvalidKernelDefinition:                        return "Invalid kernel definition";
            case ComputeErrorCode.InvalidKernel:                                  return "Invalid kernel";
            case ComputeErrorCode.InvalidArgumentIndex:                           return "Invalid argument index";
            case ComputeErrorCode.InvalidArgumentValue:                           return "Invalid argument value";
            case ComputeErrorCode.InvalidArgumentSize:                            return "Invalid argument size";
            case ComputeErrorCode.InvalidKernelArguments:                         return "Invalid kernel arguments";
            case ComputeErrorCode.InvalidWorkDimension:                           return "Invalid work dimension";
            case ComputeErrorCode.InvalidWorkGroupSize:                           return "Invalid work group size";
            case ComputeErrorCode.InvalidWorkItemSize:                            return "Invalid work item size";
            case ComputeErrorCode.InvalidGlobalOffset:                            return "Invalid global offset";
            case ComputeErrorCode.InvalidEventWaitList:                           return "Invalid event wait list";
            case ComputeErrorCode.InvalidEvent:                                   return "Invalid event";
            case ComputeErrorCode.InvalidOperation:                               return "Invalid operation";
            case ComputeErrorCode.InvalidGLObject:                                return "Invalid OpenGL object";
            case ComputeErrorCode.InvalidBufferSize:                              return "Invalid buffer size";
            case ComputeErrorCode.InvalidMipLevel:                                return "Invalid mip-map level";
            default:                                             return "Unknown";
        }
    }
    
    void opencl_succeed(int ret,
                        string call,
                        string file,
                        int line)
    {
        if (ret != (int) ComputeErrorCode.Success)
        {
            panic(-1, "{0}:{1}: OpenCL call\n  {2}\nfailed with error code {3} ({4})\n",
                  file, line, call, ret, opencl_error_string(ret));
        }
    }
    
    void set_preferred_platform(ref opencl_config cfg, string s) {
        cfg.preferred_platform = s;
    }
    
    void set_preferred_device(ref opencl_config cfg, string s)
    {
        int x = 0;
        int i = 0;
        if (s[0] == '#') {
            i = 1;
            while (char.IsDigit(s[i])) {
                x = x * 10 + (int) (s[i])-'0';
                i++;
            }
            // Skip trailing spaces.
            while (char.IsWhiteSpace(s[i])) {
                i++;
            }
        }
        cfg.preferred_device = s.Substring(i);
        cfg.preferred_device_num = x;
    }
    
    string opencl_platform_info(CLPlatformHandle platform,
                                ComputePlatformInfo param) {
        IntPtr req_bytes;
        OPENCL_SUCCEED(CL10.GetPlatformInfo(platform, param, IntPtr.Zero, null, out req_bytes));
    
        char[] info = new char[(int) req_bytes];
        unsafe
        {
            fixed (char* ptr = &info[0])
            {
                OPENCL_SUCCEED(CL10.GetPlatformInfo(platform, param, req_bytes, new IntPtr(ptr), null));
            }
        }
    
        return new string(info);
    }
    
    string opencl_device_info(CLDeviceHandle device,
                              ComputeDeviceInfo param) {
        IntPtr req_bytes;
        OPENCL_SUCCEED(CL10.GetDeviceInfo(device, param, 0, null, out req_bytes));
    
        char[] info = new char[(int) req_bytes];
        unsafe
        {
            fixed (char* ptr = &info[0])
            {
                OPENCL_SUCCEED(CL10.GetDeviceInfo(device, param, req_bytes, new IntPtr(ptr), null));
            }
        }
    
        return new string(info);
    }
    
    void opencl_all_device_options(out opencl_device_option[] devices_out,
                                   out int num_devices_out)
    {
        int num_devices = 0, num_devices_added = 0;
    
        CLPlatformHandle[] all_platforms;
        int[] platform_num_devices;
    
        int num_platforms;
    
        // Find the number of platforms.
        OPENCL_SUCCEED(CL10.GetPlatformIDs(0, null, out num_platforms));
    
        // Make room for them.
        all_platforms = new CLPlatformHandle[num_platforms];
        platform_num_devices = new int[num_platforms];
    
        int tmp;
        // Fetch all the platforms.
        OPENCL_SUCCEED(CL10.GetPlatformIDs(num_platforms, all_platforms, out tmp));
    
        // Count the number of devices for each platform, as well as the
        // total number of devices.
        for (int i = 0; i < num_platforms; i++)
        {
            if (CL10.GetDeviceIDs(all_platforms[i], ComputeDeviceTypes.All,
                                  0, null, out platform_num_devices[i]) == ComputeErrorCode.Success)
            {
                num_devices += platform_num_devices[i];
            }
            else
            {
                platform_num_devices[i] = 0;
            }
        }
    
        // Make room for all the device options.
        opencl_device_option[] devices = new opencl_device_option[num_devices];
    
        // Loop through the platforms, getting information about their devices.
        for (int i = 0; i < num_platforms; i++) {
            CLPlatformHandle platform = all_platforms[i];
            int num_platform_devices = platform_num_devices[i];
    
            if (num_platform_devices == 0) {
                continue;
            }
    
            string platform_name = opencl_platform_info(platform, ComputePlatformInfo.Name);
            CLDeviceHandle[] platform_devices = new CLDeviceHandle[num_platform_devices];
    
            // Fetch all the devices.
            OPENCL_SUCCEED(CL10.GetDeviceIDs(platform, ComputeDeviceTypes.All,
                                             num_platform_devices, platform_devices, out tmp));
    
            IntPtr tmpptr;
            // Loop through the devices, adding them to the devices array.
            unsafe
            {
                for (int j = 0; i < num_platform_devices; j++) {
                    string device_name = opencl_device_info(platform_devices[j], ComputeDeviceInfo.Name);
                    devices[num_devices_added].platform = platform;
                    devices[num_devices_added].device = platform_devices[j];
                    fixed (void* ptr = &devices[num_devices_added].device_type)
                    {
                        OPENCL_SUCCEED(CL10.GetDeviceInfo(platform_devices[j],
                                                          ComputeDeviceInfo.Type,
                                                          new IntPtr(sizeof(ComputeDeviceTypes)),
                                                          new IntPtr(ptr),
                                                          out tmpptr));
                    }
                    // We don't want the structs to share memory, so copy the platform name.
                    // Each device name is already unique.
                    devices[num_devices_added].platform_name = platform_name;
                    devices[num_devices_added].device_name = device_name;
                    num_devices_added++;
                }
            }
        }
    
        devices_out = devices;
        num_devices_out = num_devices;
    }
    
    bool is_blacklisted(string platform_name, string device_name)
    {
        return (platform_name.Contains("Apple") &&
                device_name.Contains("Intel(R) Core(TM)"));
    }
    
    opencl_device_option get_preferred_device(opencl_config cfg) {
        opencl_device_option[] devices;
        int num_devices;
    
        opencl_all_device_options(out devices, out num_devices);
    
        int num_device_matches = 0;
    
        for (int i = 0; i < num_devices; i++)
        {
            opencl_device_option device = devices[i];
            if (!is_blacklisted(device.platform_name, device.device_name) &&
                device.platform_name.Contains(cfg.preferred_platform) &&
                device.device_name.Contains(cfg.preferred_device) &&
                num_device_matches++ == cfg.preferred_device_num)
            {
                return device;
            }
        }
    
        panic(1, "Could not find acceptable OpenCL device.\n");
    
    }
    
    void describe_device_option(opencl_device_option device) {
        Console.Error.WriteLine("Using platform: {0}", device.platform_name);
        Console.Error.WriteLine("Using device: {0}", device.device_name);
    }
    
    ComputeProgramBuildStatus build_opencl_program(out CLProgramHandle program, CLDeviceHandle device, string options) {
        ComputeErrorCode ret_val = CL10.BuildProgram(program, 1, new []{device}, options, null, IntPtr.Zero);
    
        // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
        if (ret_val != ComputeErrorCode.Success && ret_val != ComputeErrorCode.BuildProgramFailure) {
            Debug.Assert((int) ret_val == 0);
        }
    
        ComputeProgramBuildStatus build_status;
        unsafe
        {
            ret_val = CL10.GetProgramBuildInfo(program,
                                               device,
                                               ComputeProgramBuildInfo.Status,
                                               new IntPtr(sizeof(int)),
                                               &build_status,
                                               null);
        }
        Debug.Assert(ret_val == 0);
    
        if (build_status != ComputeProgramBuildStatus.Success) {
            char[] build_log;
            int ret_val_size;
            unsafe
            {
            ret_val = CL10.GetProgramBuildInfo(program,
                                               device,
                                               ComputeProgramBuildInfo.BuildLog,
                                               IntPtr.Zero,
                                               IntPtr.Zero,
                                               &ret_val_size);
            }
            Debug.Assert(ret_val == 0);
    
            build_log = new char[ret_val_size+1];
            IntPtr tmp;
            unsafe
            {
                fixed (char* ptr = &build_log[0])
                {
                    CL10.GetProgramBuildInfo(program,
                                             device,
                                             ComputeProgramBuildInfo.BuildLog,
                                             new IntPtr(ret_val_size),
                                             new IntPtr(ptr),
                                             out tmp);
                }
            }
            Debug.Assert(ret_val == 0);
    
            // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
            build_log[ret_val_size] = '\0';
            Console.Error.Write("Build log:\n{0}\n", new string(build_log));
        }
    
        return build_status;
    }
    
    
    // We take as input several strings representing the program, because
    // C does not guarantee that the compiler supports particularly large
    // literals.  Notably, Visual C has a limit of 2048 characters.  The
    // array must be NULL-terminated.
    CLProgramHandle setup_opencl(ref futhark_context ctx,
                                 string[] srcs,
                                 bool required_types) {
    
        int error;
        CLPlatformHandle platform;
        CLDeviceHandle device;
        int max_group_size;
    
        ctx.opencl.lockstep_width = 0;
    
        opencl_device_option device_option = get_preferred_device(ctx.opencl.cfg);
    
        if (ctx.debugging) {
            describe_device_option(device_option);
        }
    
        device = device = device_option.device;
        platform = platform = device_option.platform;
    
        if (required_types){
            int supported;
            unsafe
            {
                OPENCL_SUCCEED(CL10.GetDeviceInfo(device,
                                                  ComputeDeviceInfo.PreferredVectorWidthDouble,
                                                  new IntPtr(sizeof(int)),
                                                  &supported,
                                                  null));
            }
            if (supported == 0) {
                panic(1,
                      "Program uses double-precision floats, but this is not supported on chosen device: {0}\n",
                      device_option.device_name);
            }
        }
    
        unsafe
        {
            OPENCL_SUCCEED(CL10.GetDeviceInfo(device,
                                              ComputeDeviceInfo.MaxWorkGroupSize,
                                              new IntPtr(sizeof(int)),
                                              &max_group_size,
                                              null));
        }
    
        int max_tile_size = (int) Math.Sqrt(max_group_size);
    
        if (max_group_size < ctx.opencl.cfg.default_group_size) {
            Console.Error.WriteLine("Note: Device limits default group size to {0} (down from {1}).\n",
                                    max_group_size, ctx.opencl.cfg.default_group_size);
            ctx.opencl.cfg.default_group_size = max_group_size;
        }
    
        if (max_tile_size < ctx.opencl.cfg.default_tile_size) {
            Console.Error.WriteLine("Note: Device limits default tile size to {0} (down from {1}).\n",
                                    max_tile_size, ctx.opencl.cfg.default_tile_size);
            ctx.opencl.cfg.default_tile_size = max_tile_size;
        }
    
        ctx.opencl.max_group_size = max_group_size;
        ctx.opencl.max_tile_size = max_tile_size; // No limit.
        ctx.opencl.max_threshold = ctx.opencl.max_num_groups = 0; // No limit.
    
        // Now we go through all the sizes, clamp them to the valid range,
        // or set them to the default.
        for (int i = 0; i < ctx.opencl.cfg.num_sizes; i++) {
            string size_class = ctx.opencl.cfg.size_classes[i];
            int size_value = ctx.opencl.cfg.size_values[i];
            string size_name = ctx.opencl.cfg.size_names[i];
            int max_value, default_value;
            if (size_class == "group_size") {
                max_value = max_group_size;
                default_value = ctx.opencl.cfg.default_group_size;
            } else if (size_class == "num_groups") {
                max_value = max_group_size; // Futhark assumes this constraint.
                default_value = ctx.opencl.cfg.default_num_groups;
            } else if (size_class == "tile_size"){
                max_value = (int) Math.Sqrt(max_group_size);
                default_value = ctx.opencl.cfg.default_tile_size;
            } else if (size_class == "threshold") {
                max_value = 0; // No limit.
                default_value = ctx.opencl.cfg.default_threshold;
            } else {
                panic(1, "Unknown size class for size '{0}': {1}\n", size_name, size_class);
            }
            if (size_value == 0) {
                ctx.opencl.cfg.size_values[i] = default_value;
            } else if (max_value > 0 && size_value > max_value) {
                Console.Error.WriteLine("Note: Device limits {0} to {1} (down from {2})",
                                        size_name, max_value, size_value);
                ctx.opencl.cfg.size_values[i] = default_value;
            }
        }
    
        IntPtr[] properties = new []{
            new IntPtr((int) ComputeContextInfo.Platform),
            platform.Value,
            IntPtr.Zero
        };
        // Note that nVidia's OpenCL requires the platform property
        ctx.opencl.context = CL10.CreateContext(properties, 1, new []{device}, null, null, out error);
        Debug.Assert(error == 0);
    
        ctx.opencl.queue = CL10.CreateCommandQueue(ctx.opencl.context, device, 0, out error);
        Debug.Assert(error == 0);
    
        // Make sure this function is defined.
        post_opencl_setup(ref ctx, ref device_option);
    
        if (ctx.debugging) {
            Console.Error.WriteLine("Lockstep width: {0}\n", (int)ctx.opencl.lockstep_width);
            Console.Error.WriteLine("Default group size: {0}\n", (int)ctx.opencl.cfg.default_group_size);
            Console.Error.WriteLine("Default number of groups: {0}\n", (int)ctx.opencl.cfg.default_num_groups);
        }
    
        string fut_opencl_src;
    
        // Maybe we have to read OpenCL source from somewhere else (used for debugging).
        if (ctx.opencl.cfg.load_program_from != null) {
            fut_opencl_src = File.ReadAllText(ctx.opencl.cfg.load_program_from);
        } else {
            // Build the OpenCL program.  First we have to concatenate all the fragments.
            fut_opencl_src = string.Join("\n", srcs);
        }
    
        CLProgramHandle prog;
        error = 0;
        string[] src_ptr = new[]{fut_opencl_src};
        IntPtr[] src_size = new []{IntPtr.Zero};
    
        if (ctx.opencl.cfg.dump_program_to != null) {
            File.WriteAllText(ctx.opencl.cfg.dump_program_to, fut_opencl_src);
        }
    
        unsafe
        {
            prog = CL10.CreateProgramWithSource(ctx.opencl.context, 1, src_ptr, src_size, &error);
        }
        Debug.Assert(error == 0);
    
        int compile_opts_size = 1024;
    
        string compile_opts = String.Format("-DFUT_BLOCK_DIM={0} -DLOCKSTEP_WIDTH={1} ",
                                            ctx.opencl.cfg.transpose_block_dim,
                                            ctx.opencl.lockstep_width);
    
        for (int i = 0; i < ctx.opencl.cfg.num_sizes; i++) {
            compile_opts += String.Format("-D{0}={1} ",
                                          ctx.opencl.cfg.size_names[i],
                                          ctx.opencl.cfg.size_values[i]);
        }
    
        OPENCL_SUCCEED(build_opencl_program(out prog, device, compile_opts));
    
        return prog;
    }
    
    void panic(int exitcode, string str, params Object[] args)
    {
        var prog_name = Environment.GetCommandLineArgs()[0];
        Console.Write(String.Format("{0}:", prog_name));
        Console.Write(String.Format(str, args));
        Environment.Exit(exitcode);
    }
    string[] opencl_program = new string[]{"__kernel void dummy_kernel(__global unsigned char *dummy, int n)\n{\n    const int thread_gid = get_global_id(0);\n    \n    if (thread_gid >= n)\n        return;\n}\ntypedef char int8_t;\ntypedef short int16_t;\ntypedef int int32_t;\ntypedef long int64_t;\ntypedef uchar uint8_t;\ntypedef ushort uint16_t;\ntypedef uint uint32_t;\ntypedef ulong uint64_t;\n#define ALIGNED_LOCAL_MEMORY(m,size) __local unsigned char m[size] __attribute__ ((align))\nstatic inline int8_t add8(int8_t x, int8_t y)\n{\n    return x + y;\n}\nstatic inline int16_t add16(int16_t x, int16_t y)\n{\n    return x + y;\n}\nstatic inline int32_t add32(int32_t x, int32_t y)\n{\n    return x + y;\n}\nstatic inline int64_t add64(int64_t x, int64_t y)\n{\n    return x + y;\n}\nstatic inline int8_t sub8(int8_t x, int8_t y)\n{\n    return x - y;\n}\nstatic inline int16_t sub16(int16_t x, int16_t y)\n{\n    return x - y;\n}\nstatic inline int32_t sub32(int32_t x, int32_t y)\n{\n    return x - y;\n}\nstatic inline int64_t sub64(int64_t x, int64_t y)\n{\n    return x - y;\n}\nstatic inline int8_t mul8(int8_t x, int8_t y)\n{\n    return x * y;\n}\nstatic inline int16_t mul16(int16_t x, int16_t y)\n{\n    return x * y;\n}\nstatic inline int32_t mul32(int32_t x, int32_t y)\n{\n    return x * y;\n}\nstatic inline int64_t mul64(int64_t x, int64_t y)\n{\n    return x * y;\n}\nstatic inline uint8_t udiv8(uint8_t x, uint8_t y)\n{\n    return x / y;\n}\nstatic inline uint16_t udiv16(uint16_t x, uint16_t y)\n{\n    return x / y;\n}\nstatic inline uint32_t udiv32(uint32_t x, uint32_t y)\n{\n    return x / y;\n}\nstatic inline uint64_t udiv64(uint64_t x, uint64_t y)\n{\n    return x / y;\n}\nstatic inline uint8_t umod8(uint8_t x, uint8_t y)\n{\n    return x % y;\n}\nstatic inline uint16_t umod16(uint16_t x, uint16_t y)\n{\n    return x % y;\n}\nstatic inline uint32_t umod32(uint32_t x, uint32_t y)\n{\n    return x % y;\n}\nstatic inline uint64_t umod64(uint64_t x, uint64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t sdiv8(int8_t x, int8_t y)\n{\n    int8_t q = x / y;\n    int8_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int16_t sdiv16(int16_t x, int16_t y)\n{\n    int16_t q = x / y;\n    int16_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int32_t sdiv32(int32_t x, int32_t y)\n{\n    int32_t q = x / y;\n    int32_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int64_t sdiv64(int64_t x, int64_t y)\n{\n    int64_t q = x / y;\n    int64_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int8_t smod8(int8_t x, int8_t y)\n{\n    int8_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int16_t smod16(int16_t x, int16_t y)\n{\n    int16_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int32_t smod32(int32_t x, int32_t y)\n{\n    int32_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int64_t smod64(int64_t x, int64_t y)\n{\n    int64_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int8_t squot8(int8_t x, int8_t y)\n{\n    return x / y;\n}\nstatic inline int16_t squot16(int16_t x, int16_t y)\n{\n    return x / y;\n}\nstatic inline int32_t squot32(int32_t x, int32_t y)\n{\n    return x / y;\n}\nstatic inline int64_t squot64(int64_t x, int64_t y)\n{\n    return x / y;\n}\nstatic inline int8_t srem8(int8_t x, int8_t y)\n{\n    return x % y;\n}\nstatic inline int16_t srem16(int16_t x, int16_t y)\n{\n    return x % y;\n}\nstatic inline int32_t srem32(int32_t x, int32_t y)\n{\n    return x % y;\n}\nstatic inline int64_t srem64(int64_t x, int64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t smin8(int8_t x, int8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int16_t smin16(int16_t x, int16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int32_t smin32(int32_t x, int32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int64_t smin64(int64_t x, int64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint8_t umin8(uint8_t x, uint8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint16_t umin16(uint16_t x, uint16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint32_t umin32(uint32_t x, uint32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint64_t umin64(uint64_t x, uint64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int8_t smax8(int8_t x, int8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int16_t smax16(int16_t x, int16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int32_t smax32(int32_t x, int32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int64_t smax64(int64_t x, int64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t umax8(uint8_t x, uint8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint16_t umax16(uint16_t x, uint16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint32_t umax32(uint32_t x, uint32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint64_t umax64(uint64_t x, uint64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t shl8(uint8_t x, uint8_t y)\n{\n    return x << y;\n}\nstatic inline uint16_t shl16(uint16_t x, uint16_t y)\n{\n    return x << y;\n}\nstatic inline uint32_t shl32(uint32_t x, uint32_t y)\n{\n    return x << y;\n}\nstatic inline uint64_t shl64(uint64_t x, uint64_t y)\n{\n    return x << y;\n}\nstatic inline uint8_t lshr8(uint8_t x, uint8_t y)\n{\n    return x >> y;\n}\nstatic inline uint16_t lshr16(uint16_t x, uint16_t y)\n{\n    return x >> y;\n}\nstatic inline uint32_t lshr32(uint32_t x, uint32_t y)\n{\n    return x >> y;\n}\nstatic inline uint64_t lshr64(uint64_t x, uint64_t y)\n{\n    return x >> y;\n}\nstatic inline int8_t ashr8(int8_t x, int8_t y)\n{\n    return x >> y;\n}\nstatic inline int16_t ashr16(int16_t x, int16_t y)\n{\n    return x >> y;\n}\nstatic inline int32_t ashr32(int32_t x, int32_t y)\n{\n    return x >> y;\n}\nstatic inline int64_t ashr64(int64_t x, int64_t y)\n{\n    return x >> y;\n}\nstatic inline uint8_t and8(uint8_t x, uint8_t y)\n{\n    return x & y;\n}\nstatic inline uint16_t and16(uint16_t x, uint16_t y)\n{\n    return x & y;\n}\nstatic inline uint32_t and32(uint32_t x, uint32_t y)\n{\n    return x & y;\n}\nstatic inline uint64_t and64(uint64_t x, uint64_t y)\n{\n    return x & y;\n}\nstatic inline uint8_t or8(uint8_t x, uint8_t y)\n{\n    return x | y;\n}\nstatic inline uint16_t or16(uint16_t x, uint16_t y)\n{\n    return x | y;\n}\nstatic inline uint32_t or32(uint32_t x, uint32_t y)\n{\n    return x | y;\n}\nstatic inline uint64_t or64(uint64_t x, uint64_t y)\n{\n    return x | y;\n}\nstatic inline uint8_t xor8(uint8_t x, uint8_t y)\n{\n    return x ^ y;\n}\nstatic inline uint16_t xor16(uint16_t x, uint16_t y)\n{\n    return x ^ y;\n}\nstatic inline uint32_t xor32(uint32_t x, uint32_t y)\n{\n    return x ^ y;\n}\nstatic inline uint64_t xor64(uint64_t x, uint64_t y)\n{\n    return x ^ y;\n}\nstatic inline char ult8(uint8_t x, uint8_t y)\n{\n    return x < y;\n}\nstatic inline char ult16(uint16_t x, uint16_t y)\n{\n    return x < y;\n}\nstatic inline char ult32(uint32_t x, uint32_t y)\n{\n    return x < y;\n}\nstatic inline char ult64(uint64_t x, uint64_t y)\n{\n    return x < y;\n}\nstatic inline char ule8(uint8_t x, uint8_t y)\n{\n    return x <= y;\n}\nstatic inline char ule16(uint16_t x, uint16_t y)\n{\n    return x <= y;\n}\nstatic inline char ule32(uint32_t x, uint32_t y)\n{\n    return x <= y;\n}\nstatic inline char ule64(uint64_t x, uint64_t y)\n{\n    return x <= y;\n}\nstatic inline char slt8(int8_t x, int8_t y)\n{\n    return x < y;\n}\nstatic inline char slt16(int16_t x, int16_t y)\n{\n    return x < y;\n}\nstatic inline char slt32(int32_t x, int32_t y)\n{\n    return x < y;\n}\nstatic inline char slt64(int64_t x, int64_t y)\n{\n    return x < y;\n}\nstatic inline char sle8(int8_t x, int8_t y)\n{\n    return x <= y;\n}\nstatic inline char sle16(int16_t x, int16_t y)\n{\n    return x <= y;\n}\nstatic inline char sle32(int32_t x, int32_t y)\n{\n    return x <= y;\n}\nstatic inline char sle64(int64_t x, int64_t y)\n{\n    return x <= y;\n}\nstatic inline int8_t pow8(int8_t x, int8_t y)\n{\n    int8_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int16_t pow16(int16_t x, int16_t y)\n{\n    int16_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int32_t pow32(int32_t x, int32_t y)\n{\n    int32_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int64_t pow64(int64_t x, int64_t y)\n{\n    int64_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int8_t sext_i8_i8(int8_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i8_i16(int8_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i8_i32(int8_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i8_i64(int8_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i16_i8(int16_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i16_i16(int16_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i16_i32(int16_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i16_i64(int16_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i32_i8(int32_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i32_i16(int32_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i32_i32(int32_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i32_i64(int32_t x)\n{\n    return x;\n}\nstatic inline int8_t sext_i64_i8(int64_t x)\n{\n    return x;\n}\nstatic inline int16_t sext_i64_i16(int64_t x)\n{\n    return x;\n}\nstatic inline int32_t sext_i64_i32(int64_t x)\n{\n    return x;\n}\nstatic inline int64_t sext_i64_i64(int64_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i8_i8(uint8_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i8_i16(uint8_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i8_i32(uint8_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i8_i64(uint8_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i16_i8(uint16_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i16_i16(uint16_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i16_i32(uint16_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i16_i64(uint16_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i32_i8(uint32_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i32_i16(uint32_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i32_i32(uint32_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i32_i64(uint32_t x)\n{\n    return x;\n}\nstatic inline uint8_t zext_i64_i8(uint64_t x)\n{\n    return x;\n}\nstatic inline uint16_t zext_i64_i16(uint64_t x)\n{\n    return x;\n}\nstatic inline uint32_t zext_i64_i32(uint64_t x)\n{\n    return x;\n}\nstatic inline uint64_t zext_i64_i64(uint64_t x)\n{\n    return x;\n}\nstatic inline float fdiv32(float x, float y)\n{\n    return x / y;\n}\nstatic inline float fadd32(float x, float y)\n{\n    return x + y;\n}\nstatic inline float fsub32(float x, float y)\n{\n    return x - y;\n}\nstatic inline float fmul32(float x, float y)\n{\n    return x * y;\n}\nstatic inline float fmin32(float x, float y)\n{\n    return x < y ? x : y;\n}\nstatic inline float fmax32(float x, float y)\n{\n    return x < y ? y : x;\n}\nstatic inline float fpow32(float x, float y)\n{\n    return pow(x, y);\n}\nstatic inline char cmplt32(float x, float y)\n{\n    return x < y;\n}\nstatic inline char cmple32(float x, float y)\n{\n    return x <= y;\n}\nstatic inline float sitofp_i8_f32(int8_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i16_f32(int16_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i32_f32(int32_t x)\n{\n    return x;\n}\nstatic inline float sitofp_i64_f32(int64_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i8_f32(uint8_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i16_f32(uint16_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i32_f32(uint32_t x)\n{\n    return x;\n}\nstatic inline float uitofp_i64_f32(uint64_t x)\n{\n    return x;\n}\nstatic inline int8_t fptosi_f32_i8(float x)\n{\n    return x;\n}\nstatic inline int16_t fptosi_f32_i16(float x)\n{\n    return x;\n}\nstatic inline int32_t fptosi_f32_i32(float x)\n{\n    return x;\n}\nstatic inline int64_t fptosi_f32_i64(float x)\n{\n    return x;\n}\nstatic inline uint8_t fptoui_f32_i8(float x)\n{\n    return x;\n}\nstatic inline uint16_t fptoui_f32_i16(float x)\n{\n    return x;\n}\nstatic inline uint32_t fptoui_f32_i32(float x)\n{\n    return x;\n}\nstatic inline uint64_t fptoui_f32_i64(float x)\n{\n    return x;\n}\nstatic inline float futrts_log32(float x)\n{\n    return log(x);\n}\nstatic inline float futrts_sqrt32(float x)\n{\n    return sqrt(x);\n}\nstatic inline float futrts_exp32(float x)\n{\n    return exp(x);\n}\nstatic inline float futrts_cos32(float x)\n{\n    return cos(x);\n}\nstatic inline float futrts_sin32(float x)\n{\n    return sin(x);\n}\nstatic inline float futrts_acos32(float x)\n{\n    return acos(x);\n}\nstatic inline float futrts_asin32(float x)\n{\n    return asin(x);\n}\nstatic inline float futrts_atan32(float x)\n{\n    return atan(x);\n}\nstatic inline float futrts_atan2_32(float x, float y)\n{\n    return atan2(x, y);\n}\nstatic inline char futrts_isnan32(float x)\n{\n    return isnan(x);\n}\nstatic inline char futrts_isinf32(float x)\n{\n    return isinf(x);\n}\nstatic inline int32_t futrts_to_bits32(float x)\n{\n    union {\n        float f;\n        int32_t t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline float futrts_from_bits32(int32_t x)\n{\n    union {\n        int32_t f;\n        float t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\n__kernel void map_kernel_2491(int32_t sizze_2479, __global\n                              unsigned char *a_mem_2497, __global\n                              unsigned char *mem_2500)\n{\n    int32_t wave_sizze_2505;\n    int32_t group_sizze_2506;\n    char thread_active_2507;\n    int32_t gtid_2484;\n    int32_t global_tid_2491;\n    int32_t local_tid_2492;\n    int32_t group_id_2493;\n    \n    global_tid_2491 = get_global_id(0);\n    local_tid_2492 = get_local_id(0);\n    group_sizze_2506 = get_local_size(0);\n    wave_sizze_2505 = LOCKSTEP_WIDTH;\n    group_id_2493 = get_group_id(0);\n    gtid_2484 = global_tid_2491;\n    thread_active_2507 = slt32(gtid_2484, sizze_2479);\n    \n    int32_t binop_param_noncurried_2494;\n    int32_t res_2495;\n    \n    if (thread_active_2507) {\n        binop_param_noncurried_2494 = *(__global\n                                        int32_t *) &a_mem_2497[gtid_2484 * 4];\n        res_2495 = binop_param_noncurried_2494 + 2;\n    }\n    if (thread_active_2507) {\n        *(__global int32_t *) &mem_2500[gtid_2484 * 4] = res_2495;\n    }\n}\n"};
    string[] size_names = new []{"group_size_2485"};
    string[] size_classes = new []{"group_size"};
    int futhark_get_num_sizes(int i)
    {
        return 1;
    }
    string futhark_get_size_name(int i)
    {
        return size_names[i];
    }
    string futhark_get_size_class(int i)
    {
        return size_classes[i];
    }
    struct sizes{public int group_size_2485;}
    struct futhark_context_config{public opencl_config opencl;
                                  public int[] sizes;}
    futhark_context_config futhark_context_config_new()
    {
        var tmp_cfg = new futhark_context_config();
        tmp_cfg.sizes = new int[]{0};
        opencl_config_init(out tmp_cfg.opencl, 1, size_names, tmp_cfg.sizes,
                           size_classes);
        tmp_cfg.opencl.transpose_block_dim = 16;
        return tmp_cfg;
    }
    void futhark_context_config_set_debugging(ref futhark_context_config _cfg,
                                              bool flag)
    {
        _cfg.opencl.debugging = flag;
    }
    void futhark_context_config_set_device(ref futhark_context_config _cfg,
                                           string s)
    {
        set_preferred_device(ref _cfg.opencl, s);
    }
    void futhark_context_config_set_platform(ref futhark_context_config _cfg,
                                             string s)
    {
        set_preferred_platform(ref _cfg.opencl, s);
    }
    void futhark_context_config_dump_program_to(ref futhark_context_config _cfg,
                                                string path)
    {
        _cfg.opencl.dump_program_to = path;
    }
    void futhark_context_config_load_program_from(ref futhark_context_config _cfg,
                                                  string path)
    {
        _cfg.opencl.load_program_from = path;
    }
    void futhark_context_config_set_default_group_size(ref futhark_context_config _cfg,
                                                       int size)
    {
        _cfg.opencl.default_group_size = size;
    }
    void futhark_context_config_set_default_num_groups(ref futhark_context_config _cfg,
                                                       int num)
    {
        _cfg.opencl.default_num_groups = num;
    }
    void futhark_context_config_set_default_tile_size(ref futhark_context_config _cfg,
                                                      int size)
    {
        _cfg.opencl.default_tile_size = size;
    }
    void futhark_context_config_set_default_threshold(ref futhark_context_config _cfg,
                                                      int size)
    {
        _cfg.opencl.default_threshold = size;
    }
    bool futhark_context_config_set_size(ref futhark_context_config _cfg,
                                         string size_name, int size_value)
    {
        for (int i = 0 ;i < 1 ;i++)
        {
            if ((size_name == size_names[i]))
            {
                _cfg.sizes[i] = size_value;
                return true;
            }
            return false;
        }
    }
    struct futhark_context{public bool detail_memory;
                           public bool debugging;
                           public opencl_context opencl;
                           public sizes sizes;
                           public int total_runs;
                           public long total_runtime;
                           public CLKernelHandle map_kernel_2491;
                           public long map_kernel_2491_total_runtime;
                           public int map_kernel_2491_runs;}
    void post_opencl_setup(ref futhark_context ctx,
                           ref opencl_device_option option)
    {
        if (((ctx.opencl.lockstep_width == 0) && (option.platform_name.Contains("NVIDIA CUDA") && (option.device_type == ComputeDeviceTypes.Gpu))))
        {
            ctx.opencl.lockstep_width = 32;
        }
        if (((ctx.opencl.lockstep_width == 0) && (option.platform_name.Contains("AMD Accelerated Parallel Processing") && (option.device_type == ComputeDeviceTypes.Gpu))))
        {
            ctx.opencl.lockstep_width = 64;
        }
        if (((ctx.opencl.lockstep_width == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Gpu))))
        {
            ctx.opencl.lockstep_width = 1;
        }
        if (((ctx.opencl.cfg.default_num_groups == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Gpu))))
        {
            ctx.opencl.cfg.default_num_groups = 128;
        }
        if (((ctx.opencl.cfg.default_group_size == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Gpu))))
        {
            ctx.opencl.cfg.default_group_size = 256;
        }
        if (((ctx.opencl.cfg.default_tile_size == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Gpu))))
        {
            ctx.opencl.cfg.default_tile_size = 32;
        }
        if (((ctx.opencl.lockstep_width == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Cpu))))
        {
            ctx.opencl.lockstep_width = 1;
        }
        if (((ctx.opencl.cfg.default_num_groups == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Cpu))))
        {
            unsafe
            {
                CL10.GetDeviceInfo(ctx.opencl.device,
                                   ComputeDeviceInfo.MaxComputeUnits,
                                   new IntPtr(Marshal.SizeOf(ctx.opencl.cfg.default_num_groups)),
                                   &ctx.opencl.cfg.default_num_groups, null);
            }
        }
        if (((ctx.opencl.cfg.default_group_size == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Cpu))))
        {
            ctx.opencl.cfg.default_group_size = 32;
        }
        if (((ctx.opencl.cfg.default_tile_size == 0) && (option.platform_name.Contains("") && (option.device_type == ComputeDeviceTypes.Cpu))))
        {
            ctx.opencl.cfg.default_tile_size = 4;
        }
    }
    void futhark_context_new(futhark_context_config cfg)
    {
        ctx.detail_memory = cfg.opencl.debugging;
        ctx.debugging = cfg.opencl.debugging;
        ctx.opencl.cfg = cfg.opencl;
        ctx.total_runs = 0;
        ctx.total_runtime = 0;
        ctx.map_kernel_2491_total_runtime = 0;
        ctx.map_kernel_2491_runs = 0;
        var required_types = false;
        CLProgramHandle prog = setup_opencl(ref ctx, opencl_program,
                                            required_types);
        ctx.map_kernel_2491 = CL10.CreateKernel(prog, "map_kernel_2491",
                                                out error);
        Debug.Assert((error == 0), "");
        if (debugging)
        {
            Console.Error.WriteLine("Created kernel {0}", map_kernel_2491);
        }
        ctx.sizes.group_size_2485 = cfg.sizes[0];
    }
    int futhark_context_sync()
    {
        OPENCL_SUCCEED(CL10.Finish(queue));
        return 0;
    }
    (long, byte[], int) futhark_main(long a_mem_sizze_2496, byte[] a_mem_2497,
                                     int sizze_2479)
    {
        var out_memsizze_2503 = new long();
        var out_mem_2502 = allocateMem(0);
        var out_arrsizze_2504 = new int();
        int group_sizze_2486;
        //callkernel getsize v key
        var group_sizze_2486 = sizes["group_size_2485"];
        int y_2487;
        y_2487 = (group_sizze_2486 - Convert.ToInt32(1));
        int x_2488;
        x_2488 = (sizze_2479 + y_2487);
        int num_groups_2489;
        num_groups_2489 = squot32(x_2488, group_sizze_2486);
        int num_threads_2490;
        num_threads_2490 = (num_groups_2489 * group_sizze_2486);
        long binop_x_2499;
        binop_x_2499 = sext_i32_i64(sizze_2479);
        long bytes_2498;
        bytes_2498 = (binop_x_2499 * Convert.ToInt64(4));
        byte[] mem_2500;
        //allocatebuffer
        var mem_2500 = CL10.CreateBuffer(ctx.opencl.context,
                                         ComputeMemoryFlags.ReadWrite,
                                         new IntPtr(bytes_2498), IntPtr.Zero,
                                         out compute_err_code);
        //callkernel
        if (((1 * (num_groups_2489 * group_sizze_2486)) != 0))
        {
            //launchkernel
            unsafe
            {
                int kernel_arg_2508 = sizze_2479;
                CL10.SetKernelArg(map_kernel_2491_var, 0, sizeof(int),
                                  ref kernel_arg_2508);
                CL10.SetKernelArg(map_kernel_2491_var, 1, sizeof(IntPtr),
                                  ref a_mem_2497);
                CL10.SetKernelArg(map_kernel_2491_var, 2, sizeof(IntPtr),
                                  ref mem_2500);
            }
            if (((1 * (num_groups_2489 * group_sizze_2486)) != 0))
            {
                var global_work_sizze_2509 = new int[]{(num_groups_2489 * group_sizze_2486)};
                var local_work_sizze_2510 = new int[]{group_sizze_2486};
                var stop_watch_2511 = new Stopwatch();
                if (ctx.debugging)
                {
                    Console.Error.Write("Launching {0} with global work size [",
                                        "map_kernel_2491");
                    Console.Error.Write("{0}", global_work_sizze_2509[0]);
                    Console.Error.Write("] and local work size [");
                    Console.Error.Write("{0}", local_work_sizze_2510[0]);
                    Console.Error.Write("].\n");
                    stop_watch_2511.Start();
                }
                OPENCL_SUCCEED(CL10.EnqueueNDRangeKernel(ctx.opencl.queue,
                                                         map_kernel_2491_var,
                                                         new int[] {(num_groups_2489 * group_sizze_2486)},
                                                         new int[]{group_sizze_2486}));
                if (ctx.debugging)
                {
                    OPENCL_SUCCEED(CL10.Finish(ctx.opencl.queue));
                    stop_watch_2511.Stop();
                    var time_diff_2512 = stop_watch_2511.ElapsedMilliseconds;
                    ctx.map_kernel_2491_total_runtime += stop_watch_2511.ElapsedMilliseconds;
                    ctx.map_kernel_2491_runs += 1;
                    Console.Error.WriteLine("kernel {0} runtime: {1}",
                                            "map_kernel_2491", time_diff_2512);
                }
            }
            if (synchronous)
            {
                CL10.Finish(ctx.opencl.queue);
            }
        }
        out_arrsizze_2504 = sizze_2479;
        out_memsizze_2503 = bytes_2498;
        out_mem_2502 = mem_2500;
        return (out_memsizze_2503, out_mem_2502, out_arrsizze_2504);
    }
    void entry_main()
    {
        var stdin_stream = getStream();
        var a_mem_2497_ext = read_array(stdin_stream, FUTHARK_INT32, 1,
                                        Convert.ToInt32);
        var sizze_2479 = a_mem_2497_ext.shape[0];
        //allocatebuffer
        var a_mem_2497 = CL10.CreateBuffer(ctx.opencl.context,
                                           ComputeMemoryFlags.ReadWrite,
                                           new IntPtr(a_mem_sizze_2496),
                                           IntPtr.Zero, out compute_err_code);
        //packarrinput
        unsafe
        {
            fixed ((void*) ptr_2514 = &a_mem_2497_ext.array[0];)
            {
                if ((a_mem_sizze_2496 != 0))
                {
                    CL10.EnqueueWriteBuffer(ctx.opencl.queue, a_mem_2497,
                                            synchronous, new IntPtr(0),
                                            a_mem_sizze_2496,
                                            new IntPtr(ptr_2514), new IntPtr(0),
                                            null, null);
                }
            }
        }
        var out_memsizze_2503 = new long();
        var out_mem_2502 = allocateMem(0);
        var out_arrsizze_2504 = new int();
        try
        {
            if (do_warmup_run)
            {
                //allocatebuffer
                var a_mem_copy_2513 = CL10.CreateBuffer(ctx.opencl.context,
                                                        ComputeMemoryFlags.ReadWrite,
                                                        new IntPtr(a_mem_2497_ext.Length),
                                                        IntPtr.Zero,
                                                        out compute_err_code);
                //copymemory device to device
                if ((a_mem_2497_ext.Length != 0))
                {
                    CL10.EnqueueCopyBuffer(ctx.opencl.queue, a_mem_2497,
                                           a_mem_copy_2513, 0, 0,
                                           a_mem_2497_ext.Length, 0, null,
                                           null);
                }
                if (synchronous)
                {
                    CL10.Finish(ctx.opencl.queue);
                }
                (out_memsizze_2503, out_mem_2502,
                 out_arrsizze_2504) = futhark_main(a_mem_sizze_2496,
                                                   a_mem_copy_2513, sizze_2479);
            }
            for (int i = 0 ;i < num_runs ;i++)
            {
                //allocatebuffer
                var a_mem_copy_2513 = CL10.CreateBuffer(ctx.opencl.context,
                                                        ComputeMemoryFlags.ReadWrite,
                                                        new IntPtr(a_mem_2497_ext.Length),
                                                        IntPtr.Zero,
                                                        out compute_err_code);
                //copymemory device to device
                if ((a_mem_2497_ext.Length != 0))
                {
                    CL10.EnqueueCopyBuffer(ctx.opencl.queue, a_mem_2497,
                                           a_mem_copy_2513, 0, 0,
                                           a_mem_2497_ext.Length, 0, null,
                                           null);
                }
                if (synchronous)
                {
                    CL10.Finish(ctx.opencl.queue);
                }
                var stop_watch = new Stopwatch();
                stop_watch.Start();
                (out_memsizze_2503, out_mem_2502,
                 out_arrsizze_2504) = futhark_main(a_mem_sizze_2496,
                                                   a_mem_copy_2513, sizze_2479);
                stop_watch.Stop();
                var time_elapsed = stop_watch.ElapsedMilliseconds;
                if ((runtime_file != null))
                {
                    runtime_file_writer.WriteLine((time_elapsed * 1000000).ToString());
                    runtime_file_writer.WriteLine("\n");
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(String.Format("Assertion.{} failed", e));
            Environment.Exit(1);
        }
        if ((runtime_file != null))
        {
            runtime_file.Close();
        }
        if ((new int[] {out_arrsizze_2504}.Aggregate(1, (acc,
                                                         val) => (acc * val)) == 0))
        {
            Console.Write("empty(i32)");
        }
        else
        {
            var print_first_2518 = true;
            Console.Write("[");
            foreach (var print_elem_2517 in  new FlatArray<int>(tmp_arr_2515, new int[] {out_arrsizze_2504}).ForEach(Console.Write))
            {
                if (!(print_first_2518))
                {
                    Console.Write(", ");
                }
                Console.Write(String.Format("{0}i32", print_elem_2517));
                print_first_2518 = false;
            }
            Console.Write("]");
        }
        Console.Write("\n");
    }
    FileStream runtime_file;
    StreamWriter runtime_file_writer;
    bool do_warmup_run = false;
    int num_runs = 1;
    string entry_point = "main";
    futhark_context ctx;
    bool synchronous = false;
    public void internal_entry(string[] args)
    {
        var options = new OptionSet{{"t|write-runtime-to=",
                                     optarg => {if ((runtime_file != null))
                                                {
                                                    runtime_file.Close();
                                                }
                                                runtime_file = new FileStream(optarg,
                                                                              FileMode.Create);
                                                runtime_file_writer = new StreamWriter(runtime_file);}},
                                    {"r|runs=",
                                     optarg => {num_runs = Convert.ToInt32(optarg);
                                                do_warmup_run = true;}},
                                    {"e|entry-point=",
                                     optarg => {entry_point = optarg;}},
                                    {"p|platform=",
                                     optarg => {futhark_context_config_set_platform(cfg, optarg);}},
                                    {"d|device=",
                                     optarg => {futhark_context_config_set_device(cfg, optarg);}},
                                    {"dump-opencl=",
                                     optarg => {futhark_context_config_dump_program_to(cfg, optarg);}},
                                    {"load-opencl=",
                                     optarg => {futhark_context_config_load_program_from(cfg, optarg);}},
                                    {"D|debugging",
                                     optarg => {futhark_context_config_set_debugging(cfg, true);}}};
        var extra = options.Parse(args);
        var entry_points = new Dictionary<string, Action>{{"main",entry_main}};
        if (!entry_points.ContainsKey(entry_point))
        {
            Console.WriteLine(string.Format("No entry point '{}'.  Select another with --entry point.  Options are:\n{}",
                                            entry_point, string.Join("\n",
                                                                     entry_points.Keys)));
            Environment.Exit(1);
        }
        else
        {
            var entry_point_fun = entry_points[entry_point];
            entry_point_fun.Invoke();
        }
        Console.Error.WriteLine("Kernel map_kernel_2491 executed {0} times, with average runtime: {1:0.000000}\tand total runtime: {2:0.000000}",
                                map_kernel_2491_runs,
                                (((long) map_kernel_2491_total_runtime / map_kernel_2491_runs) != 0) ? map_kernel_2491_runs : 1,
                                (long) map_kernel_2491_total_runtime);
        total_runtime += map_kernel_2491_total_runtime;
        total_runs += map_kernel_2491_runs;
        if (debugging)
        {
            Console.Error.WriteLine("Ran {0} kernels with cumulative runtime: {1:0.000000}",
                                    total_runs, total_runtime);
        }
    }
}
class Program
{
    static void Main(string[] args)
    {
        var internal_instance = new FutharkInternal();
        internal_instance.internal_entry(args);
    }
}