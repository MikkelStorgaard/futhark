// Stub code for OpenCL setup.


#define OPENCL_SUCCEED(e) opencl_succeed(e, #e, __FILE__, __LINE__)
struct opencl_config
{
    int debugging;
    int preferred_device_num;
    string preferred_platform;
    string preferred_device;

    string dump_program_to;
    string load_program_from;

    int default_group_size;
    int default_num_groups;
    int default_tile_size;
    int default_threshold;
    int transpose_block_dim;

    int num_sizes;
    string[] size_names;
    int[] size_values;
    string[] size_classes;
}

void opencl_config_init(out opencl_config cfg,
                        int num_sizes,
                        string[] size_names,
                        int[] size_values,
                        string[] size_classes)
{
    cfg.debugging = 0;
    cfg.preferred_device_num = 0;
    cfg.preferred_platform = "";
    cfg.preferred_device = "";
    cfg.dump_program_to = NULL;
    cfg.load_program_from = NULL;

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
  CLPlatformHandle platform;
  CLDeviceHandle device;
  CLContextHandle ctx;
  CLCommandQueueHandle queue;

  opencl_config cfg;

  int max_group_size;
  int max_num_groups;
  int max_tile_size;
  int max_threshold;

  int lockstep_width;
};

struct opencl_device_option {
  CLPlatformHandle platform;
  CLDeviceHandle device;
  ComputeDeviceTypes device_type;
  string platform_name;
  string device_name;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */

string opencl_error_string(uint err)
{
    switch (err) {
        case Success:                            return "Success!";
        case DeviceNotFound:                   return "Device not found.";
        case DeviceNotAvailable:               return "Device not available";
        case CompilerNotAvailable:             return "Compiler not available";
        case MemoryObjectAllocationFailure:      return "Memory object allocation failure";
        case OutOfResources:                   return "Out of resources";
        case OutOfHostMemory:                 return "Out of host memory";
        case ProfilingInfoNotAvailable:       return "Profiling information not available";
        case MemoryCopyOverlap:                   return "Memory copy overlap";
        case ImageFormatMismatch:              return "Image format mismatch";
        case ImageFormatNotSupported:         return "Image format not supported";
        case BuildProgramFailure:              return "Program build failure";
        case MapFailure:                        return "Map failure";
        case InvalidValue:                      return "Invalid value";
        case InvalidDevicetype:                return "Invalid device type";
        case InvalidPlatform:                   return "Invalid platform";
        case InvalidDevice:                     return "Invalid device";
        case InvalidContext:                    return "Invalid context";
        case InvalidCommandQueueFlags:           return "Invalid queue properties";
        case InvalidCommandQueue:              return "Invalid command queue";
        case InvalidHostPointer:                   return "Invalid host pointer";
        case InvalidMemoryObject:                 return "Invalid memory object";
        case InvalidImageFormatDescriptor:    return "Invalid image format descriptor";
        case InvalidImageSize:                 return "Invalid image size";
        case InvalidSampler:                    return "Invalid sampler";
        case InvalidBinary:                     return "Invalid binary";
        case InvalidBuildOptions:              return "Invalid build options";
        case InvalidProgram:                    return "Invalid program";
        case InvalidProgramExecutable:         return "Invalid program executable";
        case InvalidKernelName:                return "Invalid kernel name";
        case InvalidKernelDefinition:          return "Invalid kernel definition";
        case InvalidKernel:                     return "Invalid kernel";
        case InvalidArgumentIndex:                  return "Invalid argument index";
        case InvalidArgumentValue:                  return "Invalid argument value";
        case InvalidArgumentSize:                   return "Invalid argument size";
        case InvalidKernelArguments:                return "Invalid kernel arguments";
        case InvalidWorkDimension:             return "Invalid work dimension";
        case InvalidWorkGroupSize:            return "Invalid work group size";
        case InvalidWorkItemSize:             return "Invalid work item size";
        case InvalidGlobalOffset:              return "Invalid global offset";
        case InvalidEventWaitList:            return "Invalid event wait list";
        case InvalidEvent:                      return "Invalid event";
        case InvalidOperation:                  return "Invalid operation";
        case InvalidGLObject:                  return "Invalid OpenGL object";
        case InvalidBufferSize:                return "Invalid buffer size";
        case InvalidMipLevel:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

static void opencl_succeed(int ret,
                           string call,
                           string file,
                           int line) {
  if (ret != CL_SUCCESS) {
    panic(-1, "%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opencl_error_string(ret));
  }
}

void set_preferred_platform(out opencl_config cfg, string s) {
  cfg.preferred_platform = s;
}

void set_preferred_device(out opencl_config cfg, string s)
{
  int x = 0;
  if (s[0] == '#') {
    int i = 1;
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
                            CompuePlatformInfo param) {
  IntPtr req_bytes;
  string info;

  OPENCL_SUCCEED(GetPlatformInfo(platform, param, 0, null, out req_bytes));

  char[] info = new[(int) req_bytes];
  fixed (char* ptr = &info[0])
  {
      OPENCL_SUCCEED(GetPlatformCover(platform, param, req_bytes, new IntPtr(ptr), null));
  }

  return new string(info);
}

static string opencl_device_info(CLDeviceHandle device,
                                 CompuePlatformInfo param) {
  IntPtr req_bytes;
  string info;

  OPENCL_SUCCEED(GetDeviceInfo(device, param, 0, null, out req_bytes));

  char[] info = new[(int) req_bytes];
  fixed (char* ptr = &info[0])
  {
      OPENCL_SUCCEED(GetDeviceInfo(platform, param, req_bytes, new IntPtr(ptr), null));
  }

  return new string(info);
}

static void opencl_all_device_options(out opencl_device_option[] devices_out,
                                      out int num_devices_out) {
  int num_devices = 0, num_devices_added = 0;

  CLPlatformHandle[] all_platforms;
  int[] platform_num_devices;

  int num_platforms;

  // Find the number of platforms.
  OPENCL_SUCCEED(GetPlatformIDs(0, null, out num_platforms));

  // Make room for them.
  all_platforms = new CLPlatformHandle[num_platforms];
  platform_num_devices = new int[num_platforms]

  // Fetch all the platforms.
  OPENCL_SUCCEED(GetPlatformIDs(num_platforms, all_platforms, out 0));

  // Count the number of devices for each platform, as well as the
  // total number of devices.
  for (int i = 0; i < num_platforms; i++)
  {
    if (GetDeviceIDs(all_platforms[i], ComputeDeviceTypes.All,
                     0, null, out platform_num_devices[i]) == CL_SUCCESS)
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

    string platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
    CLDeviceHandle[] platform_devices = new CLDeviceHandle[num_platform_devices];

    // Fetch all the devices.
    OPENCL_SUCCEED(GetDeviceIDs(platform, ComputeDeviceTypes.All,
                                num_platform_devices, platform_devices, null));

    // Loop through the devices, adding them to the devices array.
    for (int i = 0; i < num_platform_devices; i++) {
      string device_name = opencl_device_info(platform_devices[i], CL_DEVICE_NAME);
      devices[num_devices_added].platform = platform;
      devices[num_devices_added].device = platform_devices[i];
      OPENCL_SUCCEED(clGetDeviceInfo(platform_devices[i], CL_DEVICE_TYPE,
                                     sizeof(cl_device_type),
                                     &devices[num_devices_added].device_type,
                                     NULL));
      // We don't want the structs to share memory, so copy the platform name.
      // Each device name is already unique.
      devices[num_devices_added].platform_name = strclone(platform_name);
      devices[num_devices_added].device_name = device_name;
      num_devices_added++;
    }
    free(platform_devices);
    free(platform_name);
  }
  free(all_platforms);
  free(platform_num_devices);

  *devices_out = devices;
  *num_devices_out = num_devices;
}

static int is_blacklisted(const char *platform_name, const char *device_name,
                          const struct opencl_config *cfg) {
  if (strcmp(cfg->preferred_platform, "") != 0 ||
      strcmp(cfg->preferred_device, "") != 0) {
    return 0;
  } else if (strstr(platform_name, "Apple") != NULL &&
             strstr(device_name, "Intel(R) Core(TM)") != NULL) {
    return 1;
  } else {
    return 0;
  }
}

static struct opencl_device_option get_preferred_device(const struct opencl_config *cfg) {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  int num_device_matches = 0;

  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (!is_blacklisted(device.platform_name, device.device_name, cfg) &&
        strstr(device.platform_name, cfg->preferred_platform) != NULL &&
        strstr(device.device_name, cfg->preferred_device) != NULL &&
        num_device_matches++ == cfg->preferred_device_num) {
      // Free all the platform and device names, except the ones we have chosen.
      for (size_t j = 0; j < num_devices; j++) {
        if (j != i) {
          free(devices[j].platform_name);
          free(devices[j].device_name);
        }
      }
      free(devices);
      return device;
    }
  }

  panic(1, "Could not find acceptable OpenCL device.\n");
  exit(1); // Never reached
}

static void describe_device_option(struct opencl_device_option device) {
  fprintf(stderr, "Using platform: %s\n", device.platform_name);
  fprintf(stderr, "Using device: %s\n", device.device_name);
}

static cl_build_status build_opencl_program(cl_program program, cl_device_id device, const char* options) {
  cl_int ret_val = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (ret_val != CL_SUCCESS && ret_val != CL_BUILD_PROGRAM_FAILURE) {
    assert(ret_val == 0);
  }

  cl_build_status build_status;
  ret_val = clGetProgramBuildInfo(program,
                                  device,
                                  CL_PROGRAM_BUILD_STATUS,
                                  sizeof(cl_build_status),
                                  &build_status,
                                  NULL);
  assert(ret_val == 0);

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    ret_val = clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size);
    assert(ret_val == 0);

    build_log = malloc(ret_val_size+1);
    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL);
    assert(ret_val == 0);

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s\n", build_log);

    free(build_log);
  }

  return build_status;
}

/* Fields in a bitmask indicating which types we must be sure are
   available. */
enum opencl_required_type { OPENCL_F64 = 1 };

// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
static cl_program setup_opencl(struct opencl_context *ctx,
                               const char *srcs[],
                               int required_types) {

  cl_int error;
  cl_platform_id platform;
  cl_device_id device;
  size_t max_group_size;

  ctx->lockstep_width = 0;

  free_list_init(&ctx->free_list);

  struct opencl_device_option device_option = get_preferred_device(&ctx->cfg);

  if (ctx->cfg.debugging) {
    describe_device_option(device_option);
  }

  ctx->device = device = device_option.device;
  ctx->platform = platform = device_option.platform;

  if (required_types & OPENCL_F64) {
    cl_uint supported;
    OPENCL_SUCCEED(clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
                                   sizeof(cl_uint), &supported, NULL));
    if (!supported) {
      panic(1,
            "Program uses double-precision floats, but this is not supported on chosen device: %s\n",
            device_option.device_name);
    }
  }

  OPENCL_SUCCEED(clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                 sizeof(size_t), &max_group_size, NULL));

  size_t max_tile_size = sqrt(max_group_size);

  if (max_group_size < ctx->cfg.default_group_size) {
    fprintf(stderr, "Note: Device limits default group size to %zu (down from %zu).\n",
            max_group_size, ctx->cfg.default_group_size);
    ctx->cfg.default_group_size = max_group_size;
  }

  if (max_tile_size < ctx->cfg.default_tile_size) {
    fprintf(stderr, "Note: Device limits default tile size to %zu (down from %zu).\n",
            max_tile_size, ctx->cfg.default_tile_size);
    ctx->cfg.default_tile_size = max_tile_size;
  }

  ctx->max_group_size = max_group_size;
  ctx->max_tile_size = max_tile_size; // No limit.
  ctx->max_threshold = ctx->max_num_groups = 0; // No limit.

  // Now we go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    size_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    size_t max_value, default_value;
    if (strcmp(size_class, "group_size") == 0) {
      max_value = max_group_size;
      default_value = ctx->cfg.default_group_size;
    } else if (strcmp(size_class, "num_groups") == 0) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg.default_num_groups;
    } else if (strcmp(size_class, "tile_size") == 0) {
      max_value = sqrt(max_group_size);
      default_value = ctx->cfg.default_tile_size;
    } else if (strcmp(size_class, "threshold") == 0) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_threshold;
    } else {
      panic(1, "Unknown size class for size '%s': %s\n", size_name, size_class);
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }

  cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (cl_context_properties)platform,
    0
  };
  // Note that nVidia's OpenCL requires the platform property
  ctx->ctx = clCreateContext(properties, 1, &device, NULL, NULL, &error);
  assert(error == 0);

  ctx->queue = clCreateCommandQueue(ctx->ctx, device, 0, &error);
  assert(error == 0);

  // Make sure this function is defined.
  post_opencl_setup(ctx, &device_option);

  if (ctx->cfg.debugging) {
    fprintf(stderr, "Lockstep width: %d\n", (int)ctx->lockstep_width);
    fprintf(stderr, "Default group size: %d\n", (int)ctx->cfg.default_group_size);
    fprintf(stderr, "Default number of groups: %d\n", (int)ctx->cfg.default_num_groups);
  }

  char *fut_opencl_src = NULL;
  size_t src_size = 0;

  // Maybe we have to read OpenCL source from somewhere else (used for debugging).
  if (ctx->cfg.load_program_from != NULL) {
    FILE *f = fopen(ctx->cfg.load_program_from, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    src_size = ftell(f);
    fseek(f, 0, SEEK_SET);
    fut_opencl_src = malloc(src_size);
    fread(fut_opencl_src, 1, src_size, f);
    fclose(f);
  } else {
    // Build the OpenCL program.  First we have to concatenate all the fragments.
    for (const char **src = srcs; src && *src; src++) {
      src_size += strlen(*src);
    }

    fut_opencl_src = malloc(src_size + 1);

    size_t n, i;
    for (i = 0, n = 0; srcs && srcs[i]; i++) {
      strncpy(fut_opencl_src+n, srcs[i], src_size-n);
      n += strlen(srcs[i]);
    }
    fut_opencl_src[src_size] = 0;

  }

  cl_program prog;
  error = 0;
  const char* src_ptr[] = {fut_opencl_src};

  if (ctx->cfg.dump_program_to != NULL) {
    FILE *f = fopen(ctx->cfg.dump_program_to, "w");
    assert(f != NULL);
    fputs(fut_opencl_src, f);
    fclose(f);
  }

  prog = clCreateProgramWithSource(ctx->ctx, 1, src_ptr, &src_size, &error);
  assert(error == 0);

  int compile_opts_size = 1024;
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    compile_opts_size += strlen(ctx->cfg.size_names[i]) + 20;
  }
  char *compile_opts = malloc(compile_opts_size);

  int w = snprintf(compile_opts, compile_opts_size,
                   "-DFUT_BLOCK_DIM=%d -DLOCKSTEP_WIDTH=%d ",
                   (int)ctx->cfg.transpose_block_dim,
                   (int)ctx->lockstep_width);

  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    w += snprintf(compile_opts+w, compile_opts_size-w,
                  "-D%s=%d ", ctx->cfg.size_names[i],
                  (int)ctx->cfg.size_values[i]);
  }

  OPENCL_SUCCEED(build_opencl_program(prog, device, compile_opts));
  free(compile_opts);
  free(fut_opencl_src);

  return prog;
}

