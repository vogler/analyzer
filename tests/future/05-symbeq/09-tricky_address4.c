#include<pthread.h>
#include<stdio.h>

struct s {
  int datum;
  pthread_mutex_t mutex;
} a[10];

void *t_fun(void *arg) {
  int i;
  struct s *p = &a[i];
  pthread_mutex_lock(&p->mutex);
  i++;
  a[i].datum++; // RACE!
  pthread_mutex_unlock(&p->mutex);
  return NULL;
}

int main () {
  int i;
  pthread_t t1;
  pthread_create(&t1, NULL, t_fun, NULL);
  
  pthread_mutex_lock(&a[i].mutex);
  a[i].datum++; // RACE!
  pthread_mutex_unlock(&a[i].mutex);
  return 0;
}