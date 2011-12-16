// float:	 1.5e-45   .. 3.4e38
// double:	 5.0e-324  .. 1.7e308
// long doube: 1.9e-4951 .. 1.1e4932

// float
assert(1.5e-45f > 0.0);
assert(1.5e-46f == 0.0);
assert(3.4e38f != 1.0/0.0);
assert(3.5e38f == 1.0/0.0); // inf
// max range of float
assert(3.5e38f != 3.5e38);

// double
assert(5.0e-324 > 0.0);
assert(5.0e-325 == 0.0);
assert(1.7e308 != 1.0/0.0);
assert(1.8e308 == 1.0/0.0); // inf
// max range of double
assert(1.8e308 != 1.8e308L);