/*
 * file: $HEADER$
 *
 * This file contains various defines used by Trampoline
 * for conditional compiling.
 *
 * This file has been generated automatically by goil
 * on  Tue Nov 24 16:43:29 2009G
 */

#ifndef OJB_HEADER_H
#define OJB_HEADER_H

/*
 * Configuration flags
 */


/*
 * Count of the objects used in the application.
 * These informations are used by Trampoline to avoid to
 * alloc structures when some os objects are not present.
 *
 */

/*
 * Declaration of the priority levels count
 */
#define PRIO_LEVEL_COUNT        4

/*
 * Declaration of Task object count
 */
#define TASK_COUNT              2

/*
 * Declaration of ISR2 object count
 */
#define ISR_COUNT               1

/*
 * Declaration of Alarm object count
 */
#define ALARM_COUNT             1

/*
 * Declaration of Resource object count
 */
#define RESOURCE_COUNT          3

/*
 * Declaration of Send Message object count
 */
#define SEND_MESSAGE_COUNT      0

/*
 * Declaration of Receive Message object count
 */
#define RECEIVE_MESSAGE_COUNT   0

#ifdef WITH_AUTOSAR
/*
 * Declaration of Schedule Table object count
 */
#define SCHEDTABLE_COUNT        $SCHEDTABLE_COUNT$

/*
 * Declaration of Software Counters object count
 */
#define COUNTER_COUNT           $COUNTER_COUNT$

/*
 * Declaration of the number of virtual watchdogs used for timing protection
 */
#define MAXIMUM_SCHEDULED_WATCHDOGS     $MAX_WATCH_DOG$
#endif

/*
 * Define of RES_SCHEDULER priority
 */
#define RES_SCHEDULER_PRIORITY  2

#endif

/* End of file $HEADER$ */
