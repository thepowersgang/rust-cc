#if defined(__INCLUDED_POSIX_PWD_H) && defined(__INCLUDED_POSIX_SYS_TYPES_H)
#pragma once

struct passwd {
	char   *pw_name;       /* username */
	char   *pw_passwd;     /* user password */
	uid_t   pw_uid;        /* user ID */
	gid_t   pw_gid;        /* group ID */
	char   *pw_gecos;      /* user information */
	char   *pw_dir;        /* home directory */
	char   *pw_shell;      /* shell program */
};

extern struct passwd *getpwnam(const char *name);
extern struct passwd *getpwuid(uid_t uid);

#endif
