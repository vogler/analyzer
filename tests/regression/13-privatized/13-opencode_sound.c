// SKIP PARAM: --with shape --nonstatic
#include<linuxlight.h>

LIST_HEAD (usb_bus_list);
DEFINE_MUTEX(usb_bus_list_lock);
DEFINE_MUTEX(otherlock);

struct usb_bus {
  struct list_head bus_list;
  int busnum;
};

int usb_register_bus(struct usb_bus *bus) {
  mutex_lock(&usb_bus_list_lock);
  list_add (&bus->bus_list, &usb_bus_list);
  mutex_unlock(&usb_bus_list_lock);
  return 0;
}

int access1(struct usb_bus *bus) {
  mutex_lock(&otherlock);
  bus->busnum = 42; // RACE
  mutex_unlock(&otherlock);
  return 0;
}

int access2() {
  struct usb_bus *bus;
  mutex_lock(&usb_bus_list_lock);
  list_for_each_entry(bus, &usb_bus_list, bus_list) {
    bus->busnum = 42; // RACE
  }
  mutex_unlock(&usb_bus_list_lock);
  return 0;
}
