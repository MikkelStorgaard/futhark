public class FlatArray<T>
{
    private long[] shape;
    private T[] array;
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
