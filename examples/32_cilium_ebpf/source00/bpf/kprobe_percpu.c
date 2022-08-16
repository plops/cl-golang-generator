// +build ignore
#include "../headers/common.h"
struct bpf_map_def SEC("maps") kprobe_map = {.type = BPF_MAP_TYPE_PERCPU_ARRAY,
                                             .key_size = sizeof(u32),
                                             .value_size = sizeof(u64),
                                             .max_entries = 1};
SEC("kprobe/sys_execve") int kprobe_execve() {
  u32 key = 0;
  u64 initval = 1;
  u64 *valp = bpf_map_lookup_elem(&kprobe_map, &key);
  if (!(valp)) {
    bpf_map_update_elem(&kprobe_map, &key, &initval, BPF_ANY);
    return 0;
  }
  __sync_fetch_and_add(valp, 1);
  return 0;
};