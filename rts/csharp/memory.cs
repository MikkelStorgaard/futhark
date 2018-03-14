public class FlatArray<T>
{
    private int[] shape;
    private T[] array;
    public FlatArray(T[] data_array, int[] shape_array) 
    {
        shape = shape_array;
        array = data_array;
    }
    
    public FlatArray(T[] data_array)
    {
        shape = new[] {data_array.Length};
        array = data_array;
    }

    private int getIdx(int[] idxs)
    {
        int idx = 0;
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
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, byte value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, short value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, ushort value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, int value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, uint value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, long value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, ulong value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, float value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, double value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
void writeScalarArray(byte[] dest, int offset, bool value)
{
    var asBytes = BitConverter.GetBytes(value);
    Array.Copy(asBytes, 0, dest, offset, asBytes.Length);
}
