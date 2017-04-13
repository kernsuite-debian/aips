/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/*      Routines callable from Fortran to:
 *      iargc() - int fun that gets the number of command line argument
 *      getarg(narg,string) - subr that gets the command line arguments
 *      getenv(varname,string) - subr that gets environmental variables
 *      getpid() - integer function that gets the process id
 *      getuid() - integer function that gets the uid
 *      getcwd(dirname) - func that gets the current working directory
 *      getlog(username) - subroutine that gets the users name
 *      AH - Mon Jan 29 15:43:05 PST 1990
 */
 
#define goodarg(x) ( ( (unsigned int) x ) < 0xfffffffe)
 
/* copy c_source which is a C string to
 * f_dest which is a Fortran chara cter variable of length *plen
 * pad with blanks if needed
 */
static void
copy_tofort(char *f_dest, char *c_source ,int *plen)
{
        int j, len = 0, n= 0;
        extern int strlen(const char * s);
 
        /* note c_source is 0 if getenv etc failed */
        if (c_source)
                n = strlen(c_source);
        if( goodarg(plen))
                len = *plen;
        for (j = 0; j < len; j++)
                if (j < n)
                        *f_dest++ = *c_source++;
                else *f_dest++ = ' ';
}
 
/* copy f_source which is a Fortran character variable of length *plen
 * into a C string (omitting trailing blanks)
 * Return a pointer to the string
 * if p is our result and P != 0 then caller should free(p)
 */
static char *
copy_fromfort(char *f_source ,int *plen)
{
        char *p, *q;
        extern char * strncpy( char *a,const char *b, int n);
        extern char *malloc(const unsigned int size);
 
        if (goodarg(plen)) {
                p = malloc( (*plen)+1);
                if (p) {
                        (void) strncpy(p,f_source,*plen);
                        q = p + *plen;
                        *q-- = '\0';
                        /* omit trailing blanks */
                        while ( (q >= p) && (*q == ' ') )
                                *q-- = '\0';
                }
        }
        else p = 0;
        return(p);
}
 
int iargc_() { extern int afbxargn; return(afbxargn); }
 
void
getarg_(int *pn, char *s, int *plen_n, int* plen_s)
{
        extern int afbxargn;
        extern char **afbxargv;
        char *p;
 
        if ( (*pn >= 0) && (*pn <= afbxargn))
                p = afbxargv[*pn];
        else p = " ";
        copy_tofort(s, p, plen_s);
}
 
 
void
getenv_(char *arg, char *res,  int *plen_arg, int *plen_res)
{
        extern char *getenv(const char *arg);
        extern void free(char *p);
        char *p;
 
        p = copy_fromfort(arg ,plen_arg);
        if (p) {
                copy_tofort(res, getenv(p),plen_res);
                (void) free(p);
        }
}
 
int getpid_() { extern int getpid(); return(getpid()); }
int getuid_() { extern int getuid(); return(getuid()); }
 
int
getcwd_(char *s, int *plen)
{
        extern char *getcwd(char *b, int n);
        extern void free(char *p);
        extern char *malloc(const unsigned int size);
        extern int errno;
        char *p = 0, *q = 0;
 
        if (goodarg(plen))
                q = malloc(*plen+2);
        if (q) {
                copy_tofort(s, p = getcwd(q,*plen+2), plen);
                (void) free(q);
        }
        if (p)
                return(0); /* return 0 for success */
        return(errno ? errno:-1);
}
/* 
void
getlog_(char *s, int *plen)
{
        extern char *getlogin();
 
        copy_tofort(s, getlogin(),plen);
}
 */
