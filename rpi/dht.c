/* SPDX-License-Identifier: Unlicense
 *
 * Written by Akce 2019.
 *
 * This is a C library version of DHT sensor reading code using the Linux GPIO chardev ioctls.
 * The goal was to see if it would perform better than the pure Chez version.
 * It does sometimes.. maybe.. and even then not by much.
 */

// Includes for open.
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <linux/gpio.h>	// gpio
#include <poll.h>	// poll
#include <stdlib.h>	// malloc
#include <sys/ioctl.h>	// ioctl
#include <unistd.h>	// close, read, usleep

#include <sched.h>
#include <stdio.h>

typedef struct
    {
    int chipfd;
    int linefd;
    int gpio_num;
    } dht_t;

/*
 * Open GPIO line handle, OUTPUT/HI.
 */
static inline int
gpio_open_output_hi(int chipfd, int gpio_num)
    {
    int rc = 0;
    struct gpiohandle_request gr = {
        //.fd = chipfd,
        .flags = GPIOHANDLE_REQUEST_OUTPUT,
        .lines = 1,
        .lineoffsets[0] = gpio_num,
        .default_values[0] = 1,
        };

    rc = ioctl(chipfd, GPIO_GET_LINEHANDLE_IOCTL, &gr);
    if (rc != -1)
        {
        rc = gr.fd;
        }
    return rc;
    }

static inline void
gpio_set(int linefd, int value)
    {
    struct gpiohandle_data data = {
        .values[0] = value,
        };
    ioctl(linefd, GPIOHANDLE_SET_LINE_VALUES_IOCTL, &data);
#if 0
    if (ioctl(linefd, GPIOHANDLE_SET_LINE_VALUES_IOCTL, &data) == -1)
        {
        perror("gpio_set");
        }
#endif
    }

void
dht_print(const dht_t* dht)
    {
    printf("(dht %d %d %d)\n", dht->chipfd, dht->linefd, dht->gpio_num);
    }

/*
 * Open DHT device and leave in low power mode. ie, OUTPUT/HI.
 */
dht_t*
dht_open(int chipfd, int gpio_num)
    {
    dht_t* d = malloc(sizeof(d));
    int rc = gpio_open_output_hi(chipfd, gpio_num);
    if (rc == -1)
        {
        // error..
        free(d);
        d = 0;
        }
    else
        {
        d->chipfd = chipfd;
        d->linefd = rc;
        d->gpio_num = gpio_num;
        }

    return d;
    }

static inline void
awaken_dht(int fd)
    {
    gpio_set(fd, 0);
    usleep(1000);
    gpio_set(fd, 1);
    }

static inline int
gpio_watch(const dht_t* dht)
    {
    struct gpioevent_request gr = {
        //.fd = dht->chipfd,
        .lineoffset = dht->gpio_num,
        .handleflags = GPIOHANDLE_REQUEST_INPUT,
        .eventflags = GPIOEVENT_REQUEST_FALLING_EDGE,
        //.eventflags = GPIOEVENT_REQUEST_BOTH_EDGES,
        .consumer_label = "dw",		// dw = dht watch.
        };
    int rc;

    if (ioctl(dht->chipfd, GPIO_GET_LINEEVENT_IOCTL, &gr) == -1)
        {
        perror("gpio_watch");
        rc = -1;
        }
    else
        {
        rc = gr.fd;
        }

    return rc;
    }

static void
nice_max(void)
    {
    struct sched_param sched = {0};
    sched.sched_priority = sched_get_priority_max(SCHED_FIFO);
    sched_setscheduler(0, SCHED_FIFO, &sched);
    }

static void
nice_normal(void)
    {
    struct sched_param sched = {0};
    sched.sched_priority = 0;
    sched_setscheduler(0, SCHED_OTHER, &sched);
    }

int
dht_read(dht_t* dht, u_int64_t* timestamps, int* types)
    {
    int pr = 0;
    int i = 0;
    struct pollfd pfd = {
        .events = POLLIN,
        .revents = 0,
        };
    struct gpioevent_data gd;

    nice_max();
    awaken_dht(dht->linefd);
    close(dht->linefd);
    pfd.fd = gpio_watch(dht);

    // Read events.
    do {
        pr = poll(&pfd, 1, 1);
        if (pr > 0)
            {
            read(pfd.fd, &gd, sizeof(gd));
            timestamps[i] = gd.timestamp;
            types[i] = gd.id;
            }
        else if (pr == 0)
            {
            // timeout.
            if (i == 0)
                {
                printf("timeout i = %d\n", i);
                i = -1;
                }
            break;
            }
        else
            {
            // error.
            perror("poll");
            i = -2;
            break;
            }
        ++i;
        }
    while (i < 90);

    // Restore DHT to low power mode.
    nice_normal();
    close(pfd.fd);
    dht->linefd = gpio_open_output_hi(dht->chipfd, dht->gpio_num);

    return i;
    }

void
dht_close(dht_t* dht)
    {
    free(dht);
    }

