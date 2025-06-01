#ifndef TUNTAP_NIF_H
#define TUNTAP_NIF_H

void *get_device_resource(ErlNifEnv* env, ERL_NIF_TERM term, void** ptr)
  meta *meta_data = enif_priv_data(env);
  ErlNifResourceType *device_type = meta_data->device;

  enif_get_resource(env, term, device_type, ptr)
  return NULL; 

#endif 
