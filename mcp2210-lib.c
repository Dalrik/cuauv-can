/*
 * MCP2210 kernel-/user-space library
 *
 * Copyright (c) 2013 Daniel Santos <daniel.santos@pobox.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */


#ifdef __KERNEL__
# include <linux/kernel.h>
# include <linux/errno.h>
# include <linux/module.h>
# include <linux/ctype.h>
#else
# include <stddef.h>
# include <stdlib.h>
# include <stdio.h>
# include <string.h>
# include <errno.h>
# include <ctype.h>
#endif /* __KERNEL__ */

#include "mcp2210.h"
#include "mcp2210-debug.h"

static void copy_board_config_string(const char *strings, size_t strings_size,
				     size_t *pos, const char **dest,
				     const char *src)
{
	ssize_t buf_size = strings_size - *pos;

	if (!src)
		return;

	BUG_ON(buf_size < 0);

	*dest = (char *)&strings[*pos];
	strncpy((char *)*dest, src, buf_size);
	*pos += strlen(src) + 1;
}

/**
 * copy_board_config - copy a struct mcp2210_board_config object
 *
 * These things are pesky because of the strings and I wasn't in the mood to
 * make static sized strings (maybe I should have?). The main thing that this
 * function does is to collect the strings (regardless of where they are stored
 * in the source struct), copy them into the blob at the end of the struct and
 * correctly set the pointers for each.
 */
struct mcp2210_board_config *copy_board_config(
		struct mcp2210_board_config *dest,
		const struct mcp2210_board_config *src, gfp_t gfp_flags)
{
	struct mcp2210_board_config *ret;
	size_t str_size = 0;
	size_t str_buffer_size;
	size_t pos = 0;
	uint i;

	for (i = 0; i < MCP2210_NUM_PINS; ++i) {
		const struct mcp2210_pin_config *pin = &src->pins[i];
		printk(KERN_DEBUG "src->pins[%u].modalias = %p (\"%s\")\n",
		       i, pin->modalias, pin->modalias ? pin->modalias : "");
		if (pin->modalias)
			str_size += strlen(pin->modalias) + 1;
	}

	if (dest) {
		if (dest->strings_size < str_size) {
			printk(KERN_ERR "need %u bytes, got %u\n",
			       (uint)str_size, (uint)dest->strings_size);
			return ERR_PTR(-EOVERFLOW);
		}
		str_buffer_size = dest->strings_size;
	} else {
		if (!(dest = kzalloc(sizeof(*ret) + str_size, gfp_flags)))
			return NULL;
		str_buffer_size = str_size;
	}

	memcpy(dest, src, sizeof(*dest));

	/* Newly allocated buffer will be exactly the size needed, supplied
	 * buffer may be larger.  Either way, we ignore the src buffer size */
	dest->strings_size = str_buffer_size;

	for (i = 0; i < MCP2210_NUM_PINS; ++i) {
		const struct mcp2210_pin_config *src_pin = &src->pins[i];
		struct mcp2210_pin_config *dest_pin = &dest->pins[i];

		copy_board_config_string(dest->strings, str_size, &pos,
					 &dest_pin->modalias,
					 src_pin->modalias);
	}

	BUG_ON(pos != str_size);

	return dest;
}



int validate_board_config(const struct mcp2210_board_config *src,
			  const struct mcp2210_chip_settings *chip)
{
	uint i;

	/* validate settings & write irq data */
	for (i = 0; i < MCP2210_NUM_PINS; ++i) {
		const struct mcp2210_pin_config *pin = &src->pins[i];

		/* a few sanity checks on the chip_settings */
		if (chip->pin_mode[i] > MCP2210_PIN_DEDICATED) {
			printk(KERN_ERR "Invalid pin mode in chip_settings\n");
			return -EINVAL;
		}

		if (pin->mode != chip->pin_mode[i]) {
			printk(KERN_ERR "chip_settings don't match "
					"board_config.\n");
			return -EINVAL;
		}

		if (pin->mode == MCP2210_PIN_DEDICATED && i != 6
						       && pin->has_irq) {
			printk(KERN_ERR "Invalid: IRQ on dedicated pin other "
					"than 6.");
			return -EINVAL;
		}
	}

	/* validate IRQ consumers match producers */
	for (i = 0; i < MCP2210_NUM_PINS; ++i) {
		const struct mcp2210_pin_config *pin_a = &src->pins[i];
		const struct mcp2210_pin_config *pin_b;
		uint j;

		if (pin_a->mode != MCP2210_PIN_SPI || !pin_a->has_irq)
			continue;

		for (j = 0; j < MCP2210_NUM_PINS; ++j) {
			pin_b = &src->pins[j];
			if (pin_b->mode == MCP2210_PIN_SPI)
				continue;

			if (pin_b->has_irq && pin_b->irq == pin_a->irq)
				break;
		}

		if (j == MCP2210_NUM_PINS) {
			printk(KERN_ERR "Invalid: spi pin %u consumes IRQ "
					"offset %u, but no other pin produces "
					"it.", i, pin_a->irq);
			return -EINVAL;
		}
	}

	/* validate spi cs_gpio */
	for (i = 0; i < MCP2210_NUM_PINS; ++i) {
		const struct mcp2210_pin_config *pin = &src->pins[i];
		u8 cs_gpio = pin->spi.cs_gpio;

		if (pin->mode != MCP2210_PIN_SPI || !pin->spi.use_cs_gpio)
			continue;

		if (cs_gpio > MCP2210_NUM_PINS || src->pins[cs_gpio].mode
							!= MCP2210_PIN_GPIO) {
			printk(KERN_ERR "Invalid: spi pin %u uses gpio for "
			       "chip select, but the specified pin (%hhu) is "
			       "not valid or not a gpio", i, cs_gpio);
			return -EINVAL;
		}
	}
	return 0;
}

/******************************************************************************
 * Verbose debug support functions
 */

#ifdef CONFIG_MCP2210_DEBUG_VERBOSE
struct code_desc {
	u8 code;
	const char *name;
};

const char indent_str[41] = "                                        ";
static inline const char *get_indent(unsigned size)
{
	static const unsigned max_indent = sizeof(indent_str) - 1;

	if (size > max_indent)
		return indent_str;
	else
		return &indent_str[max_indent - size];
}


static struct code_desc mcp2210_cmd_codes[] = {
	{MCP2210_CMD_SET_NVRAM,		"MCP2210_CMD_SET_NVRAM"},
	{MCP2210_CMD_GET_NVRAM,		"MCP2210_CMD_GET_NVRAM"},
	{MCP2210_CMD_SEND_PASSWD,	"MCP2210_CMD_SEND_PASSWD"},
	{MCP2210_CMD_GET_SPI_CONFIG,	"MCP2210_CMD_GET_SPI_CONFIG"},
	{MCP2210_CMD_SET_SPI_CONFIG,	"MCP2210_CMD_SET_SPI_CONFIG"},
	{MCP2210_CMD_GET_CHIP_CONFIG,	"MCP2210_CMD_GET_CHIP_CONFIG"},
	{MCP2210_CMD_SET_CHIP_CONFIG,	"MCP2210_CMD_SET_CHIP_CONFIG"},
	{MCP2210_CMD_GET_PIN_DIR,	"MCP2210_CMD_GET_PIN_DIR"},
	{MCP2210_CMD_SET_PIN_DIR,	"MCP2210_CMD_SET_PIN_DIR"},
	{MCP2210_CMD_GET_PIN_VALUE,	"MCP2210_CMD_GET_PIN_VALUE"},
	{MCP2210_CMD_SET_PIN_VALUE,	"MCP2210_CMD_SET_PIN_VALUE"},
	{MCP2210_CMD_READ_EEPROM,	"MCP2210_CMD_READ_EEPROM"},
	{MCP2210_CMD_WRITE_EEPROM,	"MCP2210_CMD_WRITE_EEPROM"},
	{MCP2210_CMD_GET_INTERRUPTS,	"MCP2210_CMD_GET_INTERRUPTS"},
	{MCP2210_CMD_SPI_TRANSFER,	"MCP2210_CMD_SPI_TRANSFER"},
	{MCP2210_CMD_SPI_CANCEL,	"MCP2210_CMD_SPI_CANCEL"},
	{MCP2210_CMD_SPI_RELEASE,	"MCP2210_CMD_SPI_RELEASE"},
	{MCP2210_CMD_GET_STATUS,	"MCP2210_CMD_GET_STATUS"},
	{}
};

static struct code_desc mcp2210_sub_cmd_codes[] = {
	{MCP2210_NVRAM_CHIP,		"MCP2210_NVRAM_CHIP"},
	{MCP2210_NVRAM_SPI,		"MCP2210_NVRAM_SPI"},
	{MCP2210_NVRAM_KEY_PARAMS,	"MCP2210_NVRAM_KEY_PARAMS"},
	{MCP2210_NVRAM_MFG,		"MCP2210_NVRAM_MFG"},
	{MCP2210_NVRAM_MFG,		"MCP2210_NVRAM_MFG"},
	{}
};

static struct code_desc mcp2210_status_codes[] = {
	{MCP2210_STATUS_SUCCESS,	"MCP2210_STATUS_SUCCESS"},
	{MCP2210_STATUS_SPI_NOT_OWNED,	"MCP2210_STATUS_SPI_NOT_OWNED"},
	{MCP2210_STATUS_BUSY,		"MCP2210_STATUS_BUSY"},
	{MCP2210_STATUS_WRITE_FAIL,	"MCP2210_STATUS_WRITE_FAIL"},
	{MCP2210_STATUS_NO_ACCESS,	"MCP2210_STATUS_NO_ACCESS"},
	{MCP2210_STATUS_PERM_LOCKED,	"MCP2210_STATUS_PERM_LOCKED"},
	{MCP2210_STATUS_BAD_PASSWD,	"MCP2210_STATUS_BAD_PASSWD"},
	{}
};

static struct code_desc mcp2210_pin_modes[] = {
	{MCP2210_PIN_GPIO,	"gpio"},
	{MCP2210_PIN_SPI,	"spi"},
	{MCP2210_PIN_DEDICATED,	"dedicated"},
	{}
};

static struct code_desc mcp2210_eeprom_status_codes[] = {
	{MCP2210_EEPROM_UNREAD,		"unread"},
	{MCP2210_EEPROM_READ_PENDING,	"read pending"},
	{MCP2210_EEPROM_READ,		"read"},
	{MCP2210_EEPROM_DIRTY,		"dirty"},
	{}
};

static struct code_desc mcp2210_cmd_type_id_codes[] = {
	{MCP2210_CMD_TYPE_CTL,		"ctl"},
	{MCP2210_CMD_TYPE_SPI,		"spi"},
	{MCP2210_CMD_TYPE_EEPROM,	"eeprom"},
	{}
};

static struct code_desc mcp2210_state_codes[] = {
	{MCP2210_STATE_NEW,		"new"},
	{MCP2210_STATE_SUBMITTED,	"submitted"},
	{MCP2210_STATE_COMPLETE,	"done"},
	{MCP2210_STATE_DEAD,	"dead"},
	{}
};


static const char *get_code_str(const struct code_desc *code_desc, u8 code)
{
	unsigned i;
	for (i = 0; code_desc[i].name; ++i) {
		if (code == code_desc[i].code) {
			return code_desc[i].name;
		}
	}

	return "(unknown value)";
}

inline const char *get_cmd_str(u8 cmd)
{
	return get_code_str(mcp2210_cmd_codes, cmd);
}

inline const char *get_sub_cmd_str(u8 sub_cmd)
{
	return get_code_str(mcp2210_sub_cmd_codes, sub_cmd);
}

inline const char *get_status_str(u8 status)
{
	return get_code_str(mcp2210_status_codes, status);
}

inline const char *get_pin_mode_str(u8 mode)
{
	return get_code_str(mcp2210_pin_modes, mode);
}

inline const char *get_eeprom_status_str(u8 mode)
{
	return get_code_str(mcp2210_eeprom_status_codes, mode);
}

inline const char *get_cmd_type_str(u8 mode)
{
	return get_code_str(mcp2210_cmd_type_id_codes, mode);
}

inline const char *get_state_str(u8 mode)
{
	return get_code_str(mcp2210_state_codes, mode);
}

void dump_chip_settings(const char *level, unsigned indent, const char *start,
			const struct mcp2210_chip_settings *s)
{
	const char *ind = get_indent(indent + 2);
	unsigned i;
	char buf[19];

	printk("%s%s%s%p struct mcp2210_chip_settings {\n"
	       "%s.pin_mode {\n",
	       level, get_indent(indent), start, s,
	       ind);

	for (i = 0; i < MCP2210_NUM_PINS; ++i)
		printk("%s%s  [%u] = %s\n", level, ind, i,
		       get_pin_mode_str(s->pin_mode[i]));

	for (i = 0; i < 8; ++i) {
		size_t off = i * 2 + i / 4;
		snprintf(&buf[off], sizeof(buf) - off, "%02hhx ", s->password[i]);
	}
	buf[17] = 0;

	printk("%s%s}\n"
	       "%s.gpio_value           = 0x%04hx\n"
	       "%s.gpio_direction       = 0x%04hx\n"
	       "%s.other_settings       = 0x%02hhx\n"
	       "%s.nvram_access_control = 0x%02hhx\n"
	       "%s.password             = %s\n"
	       "%s}\n",
	       level, ind,
	       ind, s->gpio_value,
	       ind, s->gpio_direction,
	       ind, s->other_settings,
	       ind, s->nvram_access_control,
	       ind, buf,
	       get_indent(indent));
}

void dump_spi_xfer_settings(const char *level, unsigned indent,
			    const char *start,
			    const struct mcp2210_spi_xfer_settings *s)
{
	const char *ind = get_indent(indent + 2);

	printk("%s%s%s%p struct mcp2210_spi_xfer_setting {\n"
	       "%s.bitrate               = %u\n"
	       "%s.idle_cs               = 0x%04hx\n"
	       "%s.active_cs             = 0x%04hx\n"
	       "%s.cs_to_data_delay      = 0x%04hx\n"
	       "%s.last_byte_to_cs_delay = 0x%04hx\n"
	       "%s.delay_between_bytes   = 0x%04hx\n"
	       "%s.bytes_per_trans       = 0x%04hx\n"
	       "%s.mode                  = 0x%02hhx\n"
	       "%s}\n",
	       level, get_indent(indent), start, s,
	       ind, s->bitrate,
	       ind, s->idle_cs,
	       ind, s->active_cs,
	       ind, s->cs_to_data_delay,
	       ind, s->last_byte_to_cs_delay,
	       ind, s->delay_between_bytes,
	       ind, s->bytes_per_trans,
	       ind, s->mode,
	       get_indent(indent));
}

void dump_usb_key_params(
	const char *level, unsigned indent, const char *start,
	const struct mcp2210_usb_key_params *params)
{
	const char *ind = get_indent(indent + 2);

	printk("%s%s%s%p struct mcp2210_spi_xfer_setting {\n"
	       "%s.vid               = 0x%04hx\n"
	       "%s.pid               = 0x%04hx\n"
	       "%s.chip_power_option = 0x%02hhx\n"
	       "%s.requested_power   = %hhu (%umA)\n"
	       "%s}\n",
	       level, get_indent(indent), start, params,
	       ind, params->vid,
	       ind, params->pid,
	       ind, params->chip_power_option,
	       ind, params->requested_power, params->requested_power * 2u,
	       get_indent(indent));
}

void dump_state(const char *level, unsigned indent, const char *start,
		const struct mcp2210_state *s)
{
	const char *ind = get_indent(indent + 2);

	printk("%s%s%s%p struct mcp2210_state {\n"
	       "%s.have_chip_settings          = %hhu\n"
	       "%s.have_power_up_chip_settings = %hhu\n"
	       "%s.have_spi_settings           = %hhu\n"
	       "%s.have_power_up_spi_settings  = %hhu\n"
	       "%s.have_usb_key_params         = %hhu\n"
	       "%s.have_config                 = %hhu\n"
	       "%s.is_spi_probed               = %hhu\n"
	       "%s.is_gpio_probed              = %hhu\n",
	       level, get_indent(indent), start, s,
	       ind, s->have_chip_settings,
	       ind, s->have_power_up_chip_settings,
	       ind, s->have_spi_settings,
	       ind, s->have_power_up_spi_settings,
	       ind, s->have_usb_key_params,
	       ind, s->have_config,
	       ind, s->is_spi_probed,
	       ind, s->is_gpio_probed);


	if (!s->have_chip_settings)
		printk("%s%s.chip_settings          = (uninitialized)\n",
		       level, ind);
	else
		dump_chip_settings(level, indent + 2,
				".chip_settings = ",
				&s->chip_settings);

	if (!s->have_power_up_chip_settings)
		printk("%s%s.power_up_chip_settings = (uninitialized)\n",
		       level, ind);
	else
		dump_chip_settings(level, indent + 2,
				".power_up_chip_settings = ",
				&s->power_up_chip_settings);

	if (!s->have_spi_settings)
		printk("%s%s.spi_settings           = (uninitialized)\n",
		       level, ind);
	else
		dump_spi_xfer_settings(level, indent + 2,
				".spi_settings = ",
				&s->spi_settings);

	if (!s->have_power_up_spi_settings)
		printk("%s%s.power_up_spi_settings  = (uninitialized)\n",
		       level, ind);
	else
		dump_spi_xfer_settings(level, indent + 2,
				".power_up_spi_settings = ",
				&s->power_up_spi_settings);

	if (!s->have_usb_key_params)
		printk("%s%s.usb_key_params         = (uninitialized)\n",
		       level, ind);
	else
		dump_usb_key_params(level, indent + 2,
				".usb_key_params = ",
				&s->usb_key_params);

	printk("%s%s.cur_spi_config              = %d\n"
	       "%s.idle_cs                     = 0x%04hx\n"
	       "%s.active_cs                   = 0x%04hx\n"
	       "%s.spi_delay_per_kb            = %lu\n"
	       "%s.last_poll_gpio              = %lu\n"
	       "%s.last_poll_intr              = %lu\n"
	       "%s.interrupt_event_counter     = 0x%04hx\n"
	       "%s}\n",
	       level, ind, s->cur_spi_config,
	       ind, s->idle_cs,
	       ind, s->active_cs,
	       ind, s->spi_delay_per_kb,
	       ind, s->last_poll_gpio,
	       ind, s->last_poll_intr,
	       ind, s->interrupt_event_counter,
	       ind);
}

void dump_pin_config(const char *level, unsigned indent, const char *start,
		     const struct mcp2210_pin_config *cfg)
{
	const char *ind = get_indent(indent + 2);
	const char *ind2 = get_indent(indent + 4);

	printk("%s%s%s%p struct mcp2210_pin_config {\n"
	       "%s.mode     = 0x%02hhx (%s)\n"
	       "%s.has_irq  = 0x%02hhx\n"
	       "%s.irq      = 0x%02hhx\n"
	       "%s.irq_type = 0x%02hhx\n",
	       level, get_indent(indent), start, cfg,
	       ind, cfg->mode, get_pin_mode_str(cfg->mode),
	       ind, cfg->has_irq,
	       ind, cfg->irq,
	       ind, cfg->irq_type);

	switch (cfg->mode) {
	case MCP2210_PIN_DEDICATED:
	case MCP2210_PIN_GPIO:
		break;

	case MCP2210_PIN_SPI:
		printk("%s%s.spi {\n"
		       "%s.max_speed_hz          = %u\n"
		       "%s.min_speed_hz          = %u\n"
		       "%s.mode                  = 0x%02hhx\n"
		       "%s.bits_per_word         = %hhu\n"
		       "%s.cs_to_data_delay      = %hu\n"
		       "%s.last_byte_to_cs_delay = %hu\n"
		       "%s.delay_between_bytes   = %hu\n"
		       "%s.delay_between_xfers   = %hu\n"
		       "%s}\n",
		       level, ind,
		       ind2, cfg->spi.max_speed_hz,
		       ind2, cfg->spi.min_speed_hz,
		       ind2, cfg->spi.mode,
		       ind2, cfg->spi.bits_per_word,
		       ind2, cfg->spi.cs_to_data_delay,
		       ind2, cfg->spi.last_byte_to_cs_delay,
		       ind2, cfg->spi.delay_between_bytes,
		       ind2, cfg->spi.delay_between_xfers,
		       ind);
		break;
	};

	printk("%s%s.modalias = %s\n"
	       "%s.name     = %s\n"
	       "%s}\n",
	       level, ind, cfg->modalias,
	       ind, cfg->name,
	       get_indent(indent));
}

void dump_board_config(const char *level, unsigned indent, const char *start,
		       const struct mcp2210_board_config *bc)
{
	const char *ind = get_indent(indent + 2);
	char buf[7] = "[0] = ";
	uint i;

	printk("%s%s%s%p struct mcp2210_board_config {\n"
	       "%s.pins {\n",
	       level, get_indent(indent), start, bc,
	       ind);

	for (i = 0; i < MCP2210_NUM_PINS; ++i) {
		buf[1] = '0' + i;
		dump_pin_config(level, indent + 4, buf, &bc->pins[i]);
	}

	printk("%s%s}\n"
	       "%s.poll_gpio_usecs  = %u\n"
	       "%s.stale_gpio_usecs = %u\n"
	       "%s.poll_intr_usecs  = %u\n"
	       "%s.stale_intr_usecs = %u\n"
	       "%s.strings_size     = %u\n"
	       "%s.strings          = %p \"%s\"\n"
	       "%s}\n",
	       level, ind,
	       ind, (uint)bc->poll_gpio_usecs,
	       ind, (uint)bc->stale_gpio_usecs,
	       ind, (uint)bc->poll_intr_usecs,
	       ind, (uint)bc->stale_intr_usecs,
	       ind, (uint)bc->strings_size,
	       ind, bc->strings, bc->strings,
	       get_indent(indent));
}

#ifdef __KERNEL__
void dump_ep(const char *level, unsigned indent, const char *start,
	     const struct mcp2210_endpoint *ep)
{
	const char *ind = get_indent(indent + 2);

	printk("%s%s%s%p struct mcp2210_endpoint {\n"
	       "%s.ep                = %p\n"
	       "%s.urb               = %p\n"
	       "%s.buffer            = %p\n"
	       "%s.submit_time       = %lu\n"
	       "%s.unlink_in_process = %d\n"
	       "%s.state             = %hhu\n"
	       "%s.is_mcp_endianness = %hhu\n"
	       "%s.kill              = %hhu\n"
	       "%s.is_dir_in         = %hhu\n"
	       "%s.retry_count       = %hhu\n"
	       "%s}\n",
	       level, get_indent(indent), start, ep,
	       ind, ep->ep,
	       ind, ep->urb,
	       ind, ep->buffer,
	       ind, ep->submit_time,
	       ind, atomic_read(&ep->unlink_in_process),
	       ind, ep->state,
	       ind, ep->is_mcp_endianness,
	       ind, ep->kill,
	       ind, ep->is_dir_in,
	       ind, ep->retry_count,
	       get_indent(indent));
}

void dump_dev(const char *level, unsigned indent, const char *start,
	      const struct mcp2210_device *dev)
{
	const char *ind = get_indent(indent + 2);

	printk("%s%s%s%p struct mcp2210_device {\n"
	       "%s.udev            = %p\n"
	       "%s.intf            = %p\n"
	       "%s.spi_master      = %p\n"
//	       "%s.gpio_chip       = %p\n"
#ifdef CONFIG_MCP2210_SPI
	       "%s.dev_spinlock    = %slocked\n"
#endif
	       "%s.queue_spinlock  = %slocked\n"
#ifdef CONFIG_MCP2210_DEBUG
	       "%s.manager_running = %d\n"
#endif
	       "%s.cmd_queue       = {.next = %p, .prev = %p}\n"
	       "%s.cur_cmd         = %p\n",
	       level, get_indent(indent), start, dev,
	       ind, dev->udev,
	       ind, dev->intf,
#ifdef CONFIG_MCP2210_SPI
	       ind, dev->spi_master,
#endif
//	       ind, dev->gpio_chip,
	       ind, spin_is_locked((struct spinlock*)&dev->dev_spinlock)
		    ? "" : "un",
	       ind, spin_is_locked((struct spinlock*)&dev->queue_spinlock)
		    ? "" : "un",
#ifdef CONFIG_MCP2210_DEBUG
	       ind, atomic_read(&dev->manager_running),
#endif
	       ind, dev->cmd_queue.next, dev->cmd_queue.prev,
	       ind, dev->cur_cmd);

	dump_ep(level, indent + 2, ".eps[EP_OUT]     = ", &dev->eps[EP_OUT]);
	dump_ep(level, indent + 2, ".eps[EP_IN]      = ", &dev->eps[EP_IN]);

	printk("%s%s.dead            = %d\n"
	       "%s.debug_chatter_count         = %hhu\n"
	       "%s.spi_in_flight               = %hhu\n",
	       level, ind, dev->dead,
	       ind, dev->debug_chatter_count,
	       ind, dev->spi_in_flight);

	dump_state(level, indent + 2, ".s = ", &dev->s);

	if (dev->config)
		dump_board_config(level, indent + 2, ".config = ", dev->config);
	else
		printk("%s%s.config = (null)\n", level, ind);

	printk("%s%sTODO: eeprom here\n"
	       "%s}\n",
	       level, ind,
	       get_indent(indent));
}

void dump_cmd_head(const char *level, unsigned indent, const char *start, const struct mcp2210_cmd *cmd)
{
	const char *ind = get_indent(indent);

	printk("%s%s%s%p struct mcp2210_cmd {\n"
	       "%s  .dev            = %p\n"
	       "%s  .type           = %p (%s)\n"
	       "%s  .node           = %p struct list_head {.next = %p, .prev = %p}\n"
	       "%s  .time_queued    = %lu\n"
	       "%s  .time_started   = %lu\n"
	       "%s  .delay_until    = %lu\n"
	       "%s  .status         = %d\n"
	       "%s  .mcp_status     = 0x%02hhx\n"
	       "%s  .state          = %hhu (%s)\n"
	       "%s  .can_retry      = %hhu\n"
	       "%s  .delayed        = %hhu\n"
	       "%s  .nonatomic      = %hhu\n"
	       "%s  .repeat_count   = %d\n"
	       "%s  .complete       = %p\n"
	       "%s  .context        = %p\n"
	       "%s}\n",
	       level, ind, start, cmd,
	       ind, cmd->dev,
	       ind, cmd->type, cmd->type ? cmd->type->desc : "none",
	       ind, &cmd->node, cmd->node.next, cmd->node.prev,
	       ind, cmd->time_queued,
	       ind, cmd->time_started,
	       ind, cmd->delay_until,
	       ind, cmd->status,
	       ind, cmd->mcp_status,
	       ind, cmd->state, "",
	       ind, cmd->can_retry,
	       ind, cmd->delayed,
	       ind, cmd->nonatomic,
	       ind, cmd->repeat_count,
	       ind, cmd->complete,
	       ind, cmd->context,
	       ind);
}
#endif /* __KERNEL__ */

static inline char hex_nybble(u8 n)
{
	return n + (n < 10 ? '0' : 'a');
}

static void dump_ctl_msg(const char *level, unsigned indent, struct mcp2210_msg *msg, int is_req, int is_set_cmd)
{
	const char *ind = get_indent(indent);

	switch (msg->cmd) {
		u8 sub_cmd;

	case MCP2210_CMD_SET_NVRAM:
	case MCP2210_CMD_GET_NVRAM:
		sub_cmd = is_req ? msg->head.req.xet.sub_cmd
				 : msg->head.rep.xet.sub_cmd;
		switch (sub_cmd) {
		case MCP2210_NVRAM_SPI:
			goto spi_config;

		case MCP2210_NVRAM_CHIP:
			goto chip_config;

		case MCP2210_NVRAM_KEY_PARAMS: {
			const char *fmt =
			       "%s%s.body.%set_usb_params {\n"
			       "%s  .vid               = 0x%04hx\n"
			       "%s  .pid               = 0x%04hx\n"
			       "%s  .chip_power_option = 0x%02hhx\n"
			       "%s  .requested_power   = %u\n"
			       "%s}\n";
			if (is_set_cmd) {
				struct mcp2210_usb_key_params *body = &msg->
							body.set_usb_params;
				printk(fmt,
				       level, ind, "s",
				       ind, body->vid,
				       ind, body->pid,
				       ind, body->chip_power_option,
				       ind, body->requested_power * 2,
				       ind);
			} else {
				typeof(msg->body.get_usb_params) *body = &msg->
							body.get_usb_params;
				printk(fmt,
				       level, ind, "g",
				       ind, body->vid,
				       ind, body->pid,
				       ind, body->chip_power_option,
				       ind, body->requested_power * 2,
				       ind);
			}
			break;
		}

		case MCP2210_NVRAM_PROD:
		case MCP2210_NVRAM_MFG:
			printk("%s%s  no dump for message type\n", level, ind);
			break;

		};
		break;
	case MCP2210_CMD_SET_SPI_CONFIG:
	case MCP2210_CMD_GET_SPI_CONFIG:
spi_config:
		dump_spi_xfer_settings(level, indent + 2, ".body.spi = ", &msg->body.spi);
		break;

	case MCP2210_CMD_SET_CHIP_CONFIG:
	case MCP2210_CMD_GET_CHIP_CONFIG:
chip_config:
		dump_chip_settings(level, indent + 2, ".body.chip = ", &msg->body.chip);
		break;

	case MCP2210_CMD_SET_PIN_DIR:
	case MCP2210_CMD_GET_PIN_DIR:
	case MCP2210_CMD_SET_PIN_VALUE:
	case MCP2210_CMD_GET_PIN_VALUE:
		printk("%s%s.body.gpio = 0x%04hx\n",
		       level, ind, msg->body.gpio);
		break;

	default:
		printk("%s%s  no dump for message type\n", level, ind);
		break;
	};
}

void dump_mcp_msg(const char *level, unsigned indent, const char *start,
		  struct mcp2210_msg *msg, int is_req)
{
	const char *ind = get_indent(indent + 2);
	u8 cmd = msg->cmd;
	u8 status;
	const char *status_str;
	int is_set_cmd = 0;

	printk("%s%s%s%p struct mcp2210_msg {\n"
	       "%s.cmd = 0x%02hhx (%s)\n",
	       level, get_indent(indent), start, msg,
	       ind, cmd, get_cmd_str(cmd));

	if (is_req) {
		status = 0;
		status_str = NULL;
	} else {
		status = msg->head.rep.status;
		status_str = get_status_str(status);
	}

	switch (cmd) {
	case MCP2210_CMD_SET_NVRAM:
	case MCP2210_CMD_SET_SPI_CONFIG:
	case MCP2210_CMD_SET_CHIP_CONFIG:
	case MCP2210_CMD_SET_PIN_DIR:
	case MCP2210_CMD_SET_PIN_VALUE:
		is_set_cmd = 1;

		/* fall-through */
	case MCP2210_CMD_GET_NVRAM:
	case MCP2210_CMD_GET_SPI_CONFIG:
	case MCP2210_CMD_GET_CHIP_CONFIG:
	case MCP2210_CMD_GET_PIN_DIR:
	case MCP2210_CMD_GET_PIN_VALUE:
		if (is_req) {
			printk("%s%s.head.req.xet {\n"
			       "%s  .sub_cmd  = 0x%02hhx (%s)\n"
			       "%s  .reserved = 0x%04hx\n"
			       "%s}\n",
			       level, ind,
			       ind, msg->head.req.xet.sub_cmd,
				    get_sub_cmd_str(msg->head.req.xet.sub_cmd),
			       ind, msg->head.req.xet.reserved,
			       ind);

			if (is_set_cmd) {
				dump_ctl_msg(level, indent + 2, msg, is_req,
					     is_set_cmd);
			}
		} else {
			printk("%s%s.head.rep.xet {\n"
			       "%s  .status   = 0x%02hhx (%s)\n"
			       "%s  .sub_cmd  = 0x%02hhx (%s)\n"
			       "%s  .reserved = 0x%02hhx\n"
			       "%s}\n",
			       level, ind,
			       ind, status, status_str,
			       ind, msg->head.rep.xet.sub_cmd,
				    get_sub_cmd_str(msg->head.rep.xet.sub_cmd),
			       ind, msg->head.rep.xet.reserved,
			       ind);

			if (!is_set_cmd)
				dump_ctl_msg(level, indent + 2, msg, is_req,
					     is_set_cmd);
		}
		break;

	case MCP2210_CMD_READ_EEPROM:
	case MCP2210_CMD_WRITE_EEPROM:
		if (is_req) {
			printk("%s%s.head.req.eeprom {\n"
			       "%s  .addr     = 0x%02hhx\n"
			       "%s  .value    = 0x%02hhx\n"
			       "%s  .reserved = 0x%02hhx\n"
			       "%s}\n",
			       level, ind,
			       ind, msg->head.req.eeprom.addr,
			       ind, msg->head.req.eeprom.value,
			       ind, msg->head.req.eeprom.reserved,
			       ind);
		} else {
			printk("%s%s.head.rep.eeprom {\n"
			       "%s  .status = 0x%02hhx (%s)\n"
			       "%s  .addr   = 0x%02hhx\n"
			       "%s  .value  = 0x%02hhx\n"
			       "%s}\n",
			       level, ind,
			       ind, status, status_str,
			       ind, msg->head.rep.eeprom.addr,
			       ind, msg->head.rep.eeprom.value,
			       ind);
		}
		break;

	case MCP2210_CMD_GET_INTERRUPTS:
		if (is_req) {
			printk("%s%s.head.req.intr {\n"
			       "%s  .reset    = 0x%02hhx\n"
			       "%s  .reserved = 0x%04hx\n"
			       "%s}\n",
			       level, ind,
			       ind, msg->head.req.intr.reset,
			       ind, msg->head.req.intr.reserved,
			       ind);
		} else {
			printk("%s%s.head.rep.status = 0x%02hhx (%s)\n"
			       "%s.body.interrupt_event_counter = 0x%04hx\n",
			       level, ind, status, status_str,
			       ind, msg->body.interrupt_event_counter);
		}
		break;

	case MCP2210_CMD_SPI_TRANSFER:
		if (is_req) {
			printk("%s%s.head.req.spi {\n"
			       "%s  .size     = 0x%02hhx\n"
			       "%s  .reserved = 0x%04hx\n"
			       "%s}\n",
			       level, ind,
			       ind, msg->head.req.spi.size,
			       ind, msg->head.req.spi.reserved,
			       ind);
		} else {
			printk("%s%s.head.rep.spi {\n"
			       "%s  .status     = 0x%02hhx (%s)\n"
			       "%s  .size       = 0x%02hhx\n"
			       "%s  .spi_status = 0x%02hhx\n"
			       "%s}\n",
			       level, ind,
			       ind, status, status_str,
			       ind, msg->head.rep.spi.size,
			       ind, msg->head.rep.spi.spi_status,
			       ind);
		}
		break;

	case MCP2210_CMD_GET_STATUS:
	case MCP2210_CMD_SPI_CANCEL:
		if (!is_req) {
			printk("%s%s.head.rep.spi_status {\n"
			       "%s  .status                      = 0x%02hhx\n"
			       "%s  .release_external_req_status = 0x%02hhx\n"
			       "%s  .current_bus_owner           = 0x%02hhx\n"
			       "%s}\n"
			       "%s.body.spi_status {\n"
			       "%s  .num_pwd_gusses = 0x%02hhx\n"
			       "%s  .pwd_gussed     = 0x%02hhx\n"
			       "%s}\n",
			       level, ind,
			       ind, msg->head.rep.spi_status.status,
			       ind, msg->head.rep.spi_status
				    .release_external_req_status,
			       ind, msg->head.rep.spi_status.current_bus_owner,
			       ind,
			       ind,
			       ind, msg->body.spi_status.num_pwd_guesses,
			       ind, msg->body.spi_status.pwd_guessed,
			       ind);
		}
		break;

	case MCP2210_CMD_SPI_RELEASE:
		if (!is_req)
			printk("%s%s.head.rep.status = 0x%02hhx\n",
			       level, ind, msg->head.rep.status);
		break;

	};

	printk("%s%s}\n", level, get_indent(indent));
}

#ifdef __KERNEL__
void dump_spi_transfer(const char *level, unsigned indent, const char *start,
		       const struct spi_transfer *xfer)
{
	const char *ind = get_indent(indent);

	printk("%s%s%s%p struct spi_transfer {\n"
	       "%s  .tx_buf        = %p\n"
	       "%s  .rx_buf        = %p\n"
	       "%s  .len           = %u\n"
	       "%s  .tx_dma        = %p\n"
	       "%s  .rx_dma        = %p\n"
	       "%s  .cs_change     = %u\n"
	       "%s  .bits_per_word = %hhu\n"
	       "%s  .delay_usecs   = %hu\n"
	       "%s  .speed_hz      = %u\n"
	       "%s  .transfer_list = {.next = %p, .prev = %p}\n"
	       "%s}\n",
	       level, ind, start, xfer,
	       ind, xfer->tx_buf,
	       ind, xfer->rx_buf,
	       ind, xfer->len,
	       ind, (void*)xfer->tx_dma,
	       ind, (void*)xfer->tx_dma,
	       ind, xfer->cs_change,
	       ind, xfer->bits_per_word,
	       ind, xfer->delay_usecs,
	       ind, xfer->speed_hz,
	       ind, xfer->transfer_list.next, xfer->transfer_list.prev,
	       ind);
}

void dump_spi_message(const char *level, unsigned indent, const char *start,
		      const struct spi_message *msg)
{
	const char *ind = get_indent(indent);

	printk("%s%s%s%p struct spi_message {\n"
	       "%s  .transfers     = {.next = %p, .prev = %p}\n"
	       "%s  .spi           = %p\n"
	       "%s  .is_dma_mapped = %u\n"
	       "%s  .complete      = %p\n"
	       "%s  .context       = %p\n"
	       "%s  .actual_length = %u\n"
	       "%s  .status        = %d\n"
	       "%s  .queue         = {.next = %p, .prev = %p}\n"
	       "%s  .state         = %p\n"
	       "%s}\n",
	       level, ind, start, msg,
	       ind, msg->transfers.next, msg->transfers.prev,
	       ind, msg->spi,
	       ind, msg->is_dma_mapped,
	       ind, msg->complete,
	       ind, msg->context,
	       ind, msg->actual_length,
	       ind, msg->status,
	       ind, msg->queue.next, msg->queue.prev,
	       ind, msg->state,
	       ind);
}


void dump_spi_device(const char *level, unsigned indent, const char *start,
		     const struct spi_device *spi_dev)
{
	const char *ind = get_indent(indent);

	printk("%s%s%s%p struct spi_device {\n"
	       "%s  .dev\n"
	       "%s  .master           = %p\n"
	       "%s  .max_speed_hz     = %u\n"
	       "%s  .chip_select      = 0x%02hhx\n"
	       "%s  .mode             = 0x%02hhx\n"
	       "%s  .bits_per_word    = 0x%02hhx\n"
	       "%s  .irq              = %d\n"
	       "%s  .controller_state = %p\n"
	       "%s  .controller_data  = %p\n"
	       "%s  .modalias         = %s\n"
	       "%s}\n",
	       level, ind, start, spi_dev,
	       ind,
	       ind, spi_dev->master,
	       ind, spi_dev->max_speed_hz,
	       ind, spi_dev->chip_select,
	       ind, spi_dev->mode,
	       ind, spi_dev->bits_per_word,
	       ind, spi_dev->irq,
	       ind, spi_dev->controller_state,
	       ind, spi_dev->controller_data,
	       ind, spi_dev->modalias,
	       ind);
}

void dump_cmd_ctl(const char *level, unsigned indent, const char *start,
		  const struct mcp2210_cmd *cmd_head)
{
	struct mcp2210_device *dev = cmd_head->dev;
	struct mcp2210_cmd_ctl *cmd = (struct mcp2210_cmd_ctl *)cmd_head;
	const char *ind = get_indent(indent);

	printk("%s%s%s%p struct mcp2210_cmd_ctl {\n",
	       level, ind, start, cmd);

	dump_cmd_head(level, indent + 2, ".head             = ", cmd_head);
	dump_mcp_msg(level, indent + 2, ".req              = ", &cmd->req, true);

	printk("%s%s  .pin               = %hhu\n"
	       "%s  .is_mcp_endianness = %hhu\n"
	       "%s}\n",
	       level, ind, cmd->pin,
	       ind, cmd->is_mcp_endianness,
	       ind);

	if (cmd_head->state == MCP2210_STATE_COMPLETE) {
		dump_mcp_msg(level, indent, "Control command response = ",
			     dev->eps[EP_IN].buffer, false);

	}

	return;
}

void dump_cmd_spi(const char *level, unsigned indent, const char *start,
		  const struct mcp2210_cmd *cmd_head)
{
	//struct mcp2210_device *dev = cmd_head->dev;
	struct mcp2210_cmd_spi_msg *cmd = (void *)cmd_head;
	const char *ind = get_indent(indent);

	printk("%s%s%s%p struct mcp2210_cmd_type_spi {\n",
	       level, ind, start, cmd);

	dump_cmd_head(level, indent + 2, ".head = ", cmd_head);
	dump_spi_device(level, indent + 2, ".spi = ", cmd->spi);
	dump_spi_message(level, indent + 2, ".msg = ", cmd->msg);
	dump_spi_transfer(level, indent + 2, ".xfer = ", cmd->xfer);

	printk("%s  .pos             = %u\n"
	       "%s  .pending_unacked = %hu\n"
	       "%s  .pending_bytes   = %hu\n"
	       "%s  .busy_count      = %u\n"
	       "%s  .spi_in_flight   = %hhu\n",
	       level, cmd->pos,
	       ind, cmd->pending_unacked,
	       ind, cmd->pending_bytes,
	       ind, cmd->busy_count,
	       ind, cmd->spi_in_flight);

	if (cmd->ctl_cmd)
		dump_cmd_ctl(level, indent + 2, ".ctl_cmd = ",
			     &cmd->ctl_cmd->head);
	else
		printk("%s%s  .ctl_cmd         = (null)\n", level, ind);

	printk("%s%s}\n", level, ind);
}

void dump_cmd_eeprom(const char *level, unsigned indent, const char *start,
		     const struct mcp2210_cmd *cmd_head)
{
	struct mcp2210_cmd_eeprom *cmd = (void *)cmd_head;
	const char *ind = get_indent(indent + 2);

	printk("%s%s%s%p struct mcp2210_cmd_eeprom {\n",
	       level, get_indent(indent), start, cmd);

	dump_cmd_head(level, indent + 2, ".head = ", cmd_head);

	printk("%s%s.op        = 0x%02hhx\n"
	       "%s.addr      = 0x%02hhx\n"
	       "%s.zero_tail = 0x%02hhx\n"
	       "%s.size      = 0x%02x\n"
	       "%s}\n",
	       level, ind, cmd->op,
	       ind, cmd->addr,
	       ind, cmd->zero_tail,
	       ind, cmd->size,
	       get_indent(indent));
}

void _dump_cmd(const char *level, unsigned indent, const char *start,
	       const struct mcp2210_cmd *cmd_head)
{
	const struct mcp2210_cmd_type *type = cmd_head->type;

	/* hopefully, this can be compiled as a simple compare & jump into
	 * either type->dump or dump_cmd_head */
	if (type && type->dump)
		type->dump(level, indent, start, cmd_head);
	else
		dump_cmd_head(level, indent, start, cmd_head);
}

#endif /* __KERNEL__ */
#endif /* CONFIG_MCP2210_DEBUG_VERBOSE */

#if defined(__KERNEL__) && defined(CONFIG_MCP2210_DEBUG)
void _mcp2210_dump_urbs(struct mcp2210_device *dev, const char *level,
			int urb_mask)
{
	unsigned i;

	for (i = 0; i < 2; ++i) {
		if (urb_mask & 1 << i) {
			char buf[11];
			scnprintf(buf, sizeof(buf), "URB %s: ", urb_dir_str[i]);
			print_mcp_msg(level, buf, dev->eps[i].buffer);
		}
	}
}
#endif
