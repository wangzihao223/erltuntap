#include <string.h>
#include <stdio.h>

#include "erl_nif.h"
#include "tuntap.h"
#include "tuntap_nif.h"

struct meta 
{
  ErlNifResourceType* device;
};

typedef struct meta meta;

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  meta *meta_data = (meta*)enif_alloc(sizeof(meta));
  
  ErlNifResourceFlags flag;
  ErlNifResourceType *device_type = enif_open_resource_type(caller_env, "tuntap_nif", "device", NULL, ERL_NIF_RT_CREATE, &flag);
  meta_data->device = device_type;
  *priv_data = meta_data;
  return 0;
}

static ERL_NIF_TERM tuntap_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  meta *meta_data = enif_priv_data(env);
  ErlNifResourceType *device_type = meta_data->device;
  struct device *device =  tuntap_init();
  struct device **ptr = (struct device**)enif_alloc_resource(device_type, sizeof(struct device*));
  *ptr = device;
  ERL_NIF_TERM edevice = enif_make_resource(env, ptr);
  enif_release_resource(ptr);
  return edevice;
}

static ERL_NIF_TERM tuntap_destroy_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  meta *meta_data = enif_priv_data(env);
  ErlNifResourceType *device_type = meta_data->device;
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;

  if (enif_get_resource(env, device_erl, device_type, &ptr))
  {
    //printf("call it\n");
    struct device **device= ptr;
    tuntap_destroy(*device);

    return enif_make_atom(env, "true");
  }
  else {
    return enif_make_atom(env, "false");
  }
}

static ERL_NIF_TERM tuntap_start_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  ERL_NIF_TERM mode_erl = argv[1];
  ERL_NIF_TERM unit_erl = argv[2];
  int mode;
  int unit;
  enif_get_int(env, mode_erl, &mode);
  enif_get_int(env, unit_erl, &unit);
  void *ptr = NULL;
  get_device_resource(env, device_erl, &ptr);
  if (ptr != NULL)
  {
    struct device** device = ptr;
    char* name = tuntap_get_ifname(*device);

    int res = tuntap_start(*device, mode, unit);
    if (res>=0)
    {
      return enif_make_atom(env, "true");
    }
    printf("res is %d\n", res);
    return enif_make_int(env, res);
  }
  return enif_make_atom(env, "false");
}

static ERL_NIF_TERM tuntap_set_hwaddr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  ERL_NIF_TERM mac_addr_erl = argv[1];
  char buffer[1024];
  int len;

  len = enif_get_string(env, mac_addr_erl, buffer, 1024, ERL_NIF_UTF8);
  if (len) {
    void *ptr;
    get_device_resource(env, device_erl, &ptr);
    if (ptr)
    {
      struct device** device = ptr;
      int res;
      res = tuntap_set_hwaddr(*device, buffer);
      if (res)
      {
        return enif_make_atom(env, "true");
      }
    }
  }
  return enif_make_atom(env, "false");
}

static ERL_NIF_TERM tuntap_up_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr)
  {
    struct device** device = ptr;
    int res;
    res = tuntap_up(*device);
    if (res)
    {
      return enif_make_atom(env, "true");
    }
  }
  return enif_make_atom(env, "false");
}

static ERL_NIF_TERM tuntap_down_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr)
  {
    struct device** device = ptr;
    int res;
    res = tuntap_down(*device);
    if (res)
    {
      return enif_make_atom(env, "true");
    }
  }
  return enif_make_atom(env, "false");
}

static ERL_NIF_TERM tuntap_set_ip_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  ERL_NIF_TERM addr_erl = argv[1];
  ERL_NIF_TERM netmask_erl = argv[2];
  int netmask;
  enif_get_int(env, netmask_erl, &netmask);
  char buffer[1024];
  int len; 
  len = enif_get_string(env, addr_erl, buffer, 1024, ERL_NIF_UTF8);
  if (len)
  {
    void *ptr;
    get_device_resource(env, device_erl, &ptr);
    if (ptr != NULL)
    {
      struct device **device = ptr;
      int res = tuntap_set_ip(*device, buffer, netmask);
      if (res >= 0)
      {
        return enif_make_atom(env, "true");
      }
    }
  }
  return enif_make_atom(env, "false");
}


static ERL_NIF_TERM tuntap_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr != NULL)
  {
    struct device **device = ptr;
    //enif_make_new_binary(env, buf_length, )
    void* buff = enif_alloc(2048);
    int length = tuntap_read(*device, buff, 2048);
    ERL_NIF_TERM res;
    unsigned char *bin = enif_make_new_binary(env, length, &res);
    memcpy(bin, buff, length);
    enif_free(buff);
    return res;
  }
  return enif_make_atom(env, "nil");
}

static ERL_NIF_TERM tuntap_get_readable_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr == NULL){
    return enif_make_atom(env, "false");
  }
  struct device **device = ptr;
  int len = tuntap_get_readable(*device);
  return enif_make_int(env, len);
}


static ERL_NIF_TERM tuntap_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  ERL_NIF_TERM buffer = argv[1];
  ErlNifBinary bin;
  enif_inspect_binary(env, buffer, &bin);
  int size =  bin.size;
  void *data = (void *)bin.data;

  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr == NULL){
    return enif_make_atom(env, "false");
  }
  struct device **device = ptr;

  int write_len;
  write_len = tuntap_write(*device, data, size);
  ERL_NIF_TERM flag;
  ERL_NIF_TERM length;

  if (write_len < 0){
    flag = enif_make_atom(env, "false");
  }
  else{
    flag = enif_make_atom(env, "true");

  }
  length = enif_make_int(env, write_len);
  return enif_make_tuple(env, 2, flag, length);
}

static ERL_NIF_TERM tuntap_get_fd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr == NULL){
    return enif_make_atom(env, "false");
  }
  struct device **device = ptr;
  int fd = tuntap_get_fd(*device);
  ERL_NIF_TERM fd_elr  = enif_make_int(env, fd);
  return fd_elr;
}

static ERL_NIF_TERM tuntap_wait_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  // device, fd, pid
  ERL_NIF_TERM device_erl = argv[0];
  void *ptr;
  get_device_resource(env, device_erl, &ptr);
  if (ptr == NULL){
    return enif_make_atom(env, "false");
  }
  struct device **device = ptr;
  int fd;
  enif_get_int(env, argv[1], &fd);
  ErlNifPid pid;
  int res;
  res = enif_get_local_pid(env, argv[2], &pid);
  if (res <= 0)
  {
    return enif_make_atom(env, "false");
  }
  ERL_NIF_TERM ref = enif_make_ref(env);
  res = enif_select(env, fd, ERL_NIF_SELECT_READ, ptr, &pid, ref);
  return enif_make_tuple(env, 2, enif_make_atom(env, "true"), ref);
}

void get_device_resource(ErlNifEnv* env, ERL_NIF_TERM term, void** ptr)
{
  meta *meta_data = enif_priv_data(env);
  ErlNifResourceType *device_type = meta_data->device;

  enif_get_resource(env, term, device_type, ptr);
}

static ErlNifFunc nif_funcs[] ={
  {"tuntap_init", 0, tuntap_init_nif},
  {"tuntap_destroy", 1, tuntap_destroy_nif},
  {"tuntap_start_nif", 3, tuntap_start_nif},
  {"tuntap_set_hwaddr_nif", 2, tuntap_set_hwaddr_nif},
  {"tuntap_down_nif", 1, tuntap_down_nif},
  {"tuntap_up_nif", 1, tuntap_up_nif},
  {"tuntap_set_ip_nif", 3, tuntap_set_ip_nif},
  {"tuntap_read_nif", 1, tuntap_read_nif},
  {"tuntap_write_nif", 2, tuntap_write_nif},
  {"tuntap_get_fd_nif", 1, tuntap_get_fd_nif},
  {"tuntap_wait_read_nif", 3, tuntap_wait_read_nif},
  {"tuntap_get_readable_nif", 1, tuntap_get_readable_nif}
};

ERL_NIF_INIT(tuntap, nif_funcs, load, NULL, NULL, NULL)
