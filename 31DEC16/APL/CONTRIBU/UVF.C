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
/*
 *  Cactus file %W%
 *        Date %G%
 *
 */
#ifndef lint

static char SccsID[] = "%W%\t%G%";

#endif

/* uvf.c
   Reads a sdd file and writes a UVFITS file of an OTF map.
   Initial version -- Jeff Mangum 03/28/94
   Rewritten in 'c' by Thomas Folkers 06/24/94
 */

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <malloc.h>
#include <math.h>

/* #include "header.h" */
/* PDFL.H -- Defined HEADER and HEADER2 structures for POPS I/O */

#define getheads(a) swaps(a)
#define MAX_SCANS	2048

struct HEADER {
  short headcls;
  short oneptr;
  short twoptr;
  short thrptr;
  short fourptr;
  short fiveptr;
  short sixptr;
  short sevptr;
  short eigptr;
  short nineptr;
  short tenptr;
  short elvptr;
  short twlptr;
  short trnptr;
  short align1; /* makes class 1 align */
  short align2; /* makes class 1 align */

/* Class 1 */

  double headlen;
  double datalen;
  double scan;
  char obsid[8];
  char observer[16];
  char telescop[8];
  char projid[8];
  char object[16];
  char obsmode[8];
  char frontend[8];
  char backend[8];
  char precis[8];

/* Class 2 */

  double xpoint;
  double ypoint;
  double uxpnt;
  double uypnt;
  double ptcon[4];
  double orient;
  double focusr;
  double focusv;
  double focush;
  char pt_model[8];

/* Class 3 */

  double utdate;
  double ut;
  double lst;
  double norchan;
  double noswvar;
  double nophase;
  double cycllen;
  double samprat;
  char cl11type[8];

/* Class 4 */

  double epoch;
  double xsource;
  double ysource;
  double xref;
  double yref;
  double epocra;
  double epocdec;
  double gallong;
  double gallat;
  double az;
  double el;
  double indx;
  double indy;
  double desorg[3];
  char coordcd[8];

/* Class 5 */

  double tamb;
  double pressure;
  double humidity;
  double refrac;
  double dewpt;
  double mmh2o;

/* Class 6 */

  double scanang;
  double xzero;
  double yzero;
  double deltaxr;
  double deltayr;
  double nopts;
  double noxpts;
  double noypts;
  double xcell0;
  double ycell0;
  char frame[8];

/* Class 7 */

  double bfwhm;
  double offscan;
  double badchv;
  double rvsys;
  double velocity;
  char veldef[8];
  char typecal[8];

/* Class 8 */

  double appeff;
  double beameff;
  double antgain;
  double etal;
  double etafss;

/* Class 9 - Kitt Peak */

  double synfreq;
  double lofact;
  double harmonic;
  double loif;
  double firstif;
  double razoff;
  double reloff;
  double bmthrow;
  double bmorent;
  double baseoff;
  double obstol;
  double sideband;
  double wl;
  double gains;
  double pbeam[2];
  double mbeam[2];
  double sroff[4];
  double foffsig;
  double foffref1;
  double foffref2;

 /* Class 10 */

  double openpar[10];

 /* Class 11 */

  double noswvarf;
  double numcyc;
  double numcycf;
  double nophasef;
  double cycllenf;
  double samptimf;
  double varval01;
  double vardes01;
  char   phastb01[32];
  double varval02;
  double vardes02;
  char   phastb02[32];
  double varval03;
  double vardes03;
  char   phastb03[32];
  double varval04;
  double vardes04;
  char   phastb04[32];
  double varval05;
  double vardes05;
  char   phastb05[32];

 /* Class 12 */

  double obsfreq;
  double restfreq;
  double freqres;
  double bw;
  double trx;
  double tcal;
  double stsys;
  double rtsys;
  double tsource;
  double trms;
  double refpt;
  double x0;
  double deltax;
  double inttime;
  double noint;
  double spn;
  double tauh2o;
  double th2o;
  double tauo2;
  double to2;
  char polariz[8];
  double effint;
  char rx_info[16];

/* Class 13 */

  double nostac;
  double fscan;
  double lscan;
  double lamp;
  double lwid;
  double ili;
  double rms;
  double align3[4];
};

/* #include "sdd.h" */
/* defines SDD file, replaces pops.h */

struct BOOTSTRAP {
  long num_index_rec; /* # of records in index including the bootstrap record */
  long num_data_rec;  /* of data records which follow the index records.      */
  long bytperrec;     /* bytes per record (use 512 in making the file)        */
  long bytperent;     /* # of bytes per index entry (still 64)                */
  long num_entries_used;  /* # of index entries already used.
                         (This is actually misleading.  This field is really
                         the last index entry used.  All index entries after
                         this number are guaranteed to be empty, the index
                         entries prior to this number may or may not be empty.)
                      */
  long counter;       /* Counter which indicates whether the file has been
                         updated.   This field is used by the unipops disk
                         access routines to tell if the file has changed since
                         the last access.  It should be reset to 0 for a
                         new or reindexed file.  It should be incremented
                         by 1 EVERY TIME the file is changed.  It should
                         reset to 0 if an increment would exceed the max value
                         possible by the storage type. */
  long typesdd;       /* type of SDD: 0 is normal, 1 is records (GB only)*/
  long version;       /* SDD Version = 1 for this version.  This is the field
                         that is guaranteed to be non-zero in the bootstrap
                         block as compared to the previous SDD files. */
  char padd[480];     /* all zero */
};


struct DIRECTORY {

  long start_rec;  /* Starting Record number for the scan about to be
                      described by this index entry (the bootstrap record is
                      record 1). */
  long end_rec;    /* Last Record number for that scan. */
  float h_coord;   /* Horizontal Coordinate in degrees.
                      (the SOURCEX header word) */
  float v_coord;   /* Vertical Coordinate in degrees.
                      (the SOURCEY header word) */
  char source[16]; /* Source Name. */
  float scan;      /* Scan Number  */
  float freq_res;  /* For LINE scans: the frequency resolution in MHz
                      (the FREQRES header word)
                      For CONT scans: the slew rate of the telescope in
                      degrees/sec (appropriate
                      combination of DELTAXR and DELTAYR
                      taking into account the cos of the
                      declination of the source) */
  double rest_freq;/* For LINE scans: the rest frequency in MHz
                      (the RESTFREQ header word)
                      For CONT scans: the integration time per point in seconds.
                      (the SAMPRAT header word) */
  float lst;       /* LST in hours. */
  float ut;        /* UT date in YYY.MMDD format. */
  short obsmode;   /* Observing mode (Coded). */
  short phase_rec; /* phase and record number for GB records data.
                      Should be -1 for all normal data. */
  short pos_code;  /* Position Code. */
  short unused;
};
/*
Notes:

   The position code was moved to the end so that the alignment on word
    boundaries was still good.  The cookbook descriptions of the
    meaning of position code values is wrong.  The correct code for
    all known position codes (the COORDCD header word) follows:
*/

#define GALACTIC   1
#define X1950RADC  2
#define EPOCRADC   3
#define MEANRADC   4
#define APPRADC    5
#define APPHADC    6
#define X1950ECL   7
#define EPOCECL    8
#define MEANECL    9
#define APPECL    10
#define AZEL      11
#define USERDEF   12
#define X2000RADC 13
#define INDRADC   14

/*
     An empty COORDCD string (all blanks) implies 2 (1950RADC).

   The observing mode coding is described corectly in the cookbook.
See the include file ~unipops/test/includes/modes.h for the most
up to date listing of known observing modes and their corresponding
codes.  The file ~unipops/test/source/conline/modefn.c contains some
useful functions for using the information in modes.h.

   The phase_rec field is only used for GB records data currently.
It should be set to -1 for all other data (sdd type = 0 in the bootstrap
record).  If it is not -1, then it corresponds to 64*record_num + phase_num.

   Index entries can not extend beyond the length of a record as
indicated by the bootstrap block (in the current situation, there are
exactly 8 index entries per record, but there is no requirement that
there be exactly N index entries per record [although clearly its
desirable so that space isn't wasted] unused portions of index
records [non-bootstrap and non-data records] are undefined].

*/


#define GREG 588829

extern int errno, isnormal();
static pdopen();

int MAIN_;                                     /* bull-shit f77 bug */

struct UVDAT {
      float data;
      float offset;
      float weight;
};

struct UVDAT *uvdat;

                                   /* saved header of first map point */
struct HEADER head, onhead, scanhead;
float *data = NULL,
      *ondata = NULL,
      *offdata = NULL,
      *gainsdata = NULL;

int status, noint, gcount, iunit=15, ierr, n8 = 8, n6 = 6, n4 = 4;
char msgbuf[80];


main(argc, argv)
int argc;
char *argv[];
{
   int i, foundFirst=0, first, last, n0 = 0;
   char dataFile[256], gainFile[256], scantype[80], fitsFile[256];
   float onScanNum=0.0, scan=0.0, offScanNum=0.0, gainsScanNum=0.0,
      requestOff=0.0, requestGains=0.0, ifNumber;

   uvfaip_ (argv[0], dataFile, gainFile, &first, &last, &ifNumber,
      fitsFile, &ierr) ;

   if (ierr < 0) {
      if (argc != 7) {
         fprintf(stderr,
         "Usage: %s datafile gainfile firstscan# lastscan# %s\n",
            argv[0], "if# outfile");
         exit(3);
         }
      (void) strcpy (dataFile, argv[1]);
      (void) strcpy (gainFile, argv[2]);
      first           = atoi(argv[3]);
      last            = atoi(argv[4]);
      ifNumber = (float)atoi(argv[5]) / 100.0;
      (void)strcpy(fitsFile, argv[6]);
      }

   if (ierr <= 0) {
      for (i=first; i<=last; i++) {
         scan = (float)i + ifNumber;
         if (read_scan (scan, dataFile) > 0) {
            (void)strncpy (scantype, head.obsmode, 8);
            scantype[8] = '\0';
            if (strncmp(scantype, "LINETPMF", 8) == NULL) {
               if (saveOffData())
                  (void)exit(3);
               offScanNum = scan;
               }
            else {
               if (strncmp(scantype, "LINEOTF ", 8) == NULL) {
                  if (!foundFirst) {
                                             /*save for rest of prg*/
                     bcopy ((char *)&head, (char *)&scanhead,
                        sizeof(struct HEADER));
                     noint = (int)(scanhead.noint + 0.5);
                     if ((uvdat = (struct UVDAT *)calloc (noint,
                        sizeof(struct UVDAT))) == NULL) {
                        (void)sprintf (msgbuf,
                           "Error in  calloc(UVDAT)");
                        if (ierr == 0) {
                           zmsgwr_ (msgbuf, &n8);
                           uvfend_ (&n8) ;
                           }
                        else
                           (void)fprintf (stderr, "%s\n", msgbuf);
                        (void)exit(3);
                        }
                     goWriteFitsHeader (fitsFile);
                     foundFirst++;
                     }
                  if (saveOnData())
                     (void)exit(3);
                  onScanNum = head.scan;
                                       /* kludge until sdd file fixed */
                  requestOff = head.offscan + ifNumber;
                  if (requestOff != offScanNum) {
                     if (read_scan( requestOff, dataFile) > 0) {
                        if (saveOffData())
                           (void)exit(3);
                        offScanNum = requestOff;
                        }
                     else {
                        (void)sprintf (msgbuf,
                           "Off scan number #%.2f, NOT found in %s",
                           requestOff, dataFile);
                        if (ierr == 0) {
                           zmsgwr_ (msgbuf, &n8);
                           uvfend_ (&n8) ;
                           }
                        else
                           (void)fprintf (stderr, "%s\n", msgbuf);
                        (void)exit(3);
                        }
                     }
                  requestGains = head.gains;
                  if (requestGains != gainsScanNum) {
                     if (read_scan( requestGains, gainFile) > 0) {
                        if (saveGainsData())
                           (void)exit(3);
                        gainsScanNum = requestGains;
                        }
                     else {
                        (void)sprintf (msgbuf,
                           "Gains scan number #%.2f, NOT found in %s",
                           requestGains, gainFile);
                        if (ierr == 0) {
                           zmsgwr_ (msgbuf, &n8);
                           uvfend_ (&n8) ;
                           }
                        else
                           (void)fprintf (stderr, "%s\n", msgbuf);
                        (void)exit(3);
                        }
                     }
                  (void)sprintf (msgbuf,
                     "Writing scan #%7.2f, with %7.2f Off, %7.2f Gains",
                     onScanNum, offScanNum, gainsScanNum);
                  if (ierr == 0)
                     zmsgwr_ (msgbuf, &n4);
                  else
                     (void)printf ("%s\n", msgbuf);
                  goWriteFitsData();
                  }
               }
            }
         }
      }

   if (foundFirst) goWriteFitsTail ();

   if (data != NULL) (void)free ((char *)data);
   if (ondata != NULL) (void)free ((char *)ondata);
   if (offdata != NULL) (void)free ((char *)offdata);
   if (gainsdata != NULL) (void)free ((char *)gainsdata);

   if (ierr > 0) {
      uvfend_ (&n4) ;
      return(3);
      }
   else {
      if (ierr == 0) uvfend_ (&n0) ;
      return(0);
      }
}



saveOffData()
{
   if (offdata == NULL) {
      if ((offdata = (float *)malloc ((unsigned)head.datalen *
         sizeof(float))) == NULL){
         (void)sprintf (msgbuf, "Error in saveOffData( malloc() )");
         if (ierr == 0) {
            zmsgwr_ (msgbuf, &n8);
            uvfend_ (&n8) ;
            }
         else
            (void)fprintf (stderr, "%s\n", msgbuf);
         return(1);
         }
      }
   bcopy ((char *)data, (char *)offdata, (int)head.datalen);
   return (0);
}


saveOnData()
{
   if (ondata == NULL) {
      if ((ondata = (float *)malloc ((unsigned)head.datalen *
         sizeof(float))) == NULL) {
         (void)sprintf (msgbuf, "Error in saveOnData( malloc() )");
         if (ierr == 0) {
            zmsgwr_ (msgbuf, &n8);
            uvfend_ (&n8) ;
            }
         else
            (void)fprintf (stderr, "%s\n", msgbuf);
         return(1);
         }
      }
   bcopy ((char *)data, (char *)ondata, (int)head.datalen);
   bcopy ((char *)&head, (char *)&onhead, sizeof(struct HEADER));

   return(0);
}

saveGainsData()
{
   if (gainsdata == NULL) {
      if ((gainsdata = (float *)malloc ((unsigned)head.datalen *
         sizeof(float))) == NULL){
         (void)sprintf (msgbuf, "Error in saveGainsData( malloc() )");
         if (ierr == 0) {
            zmsgwr_ (msgbuf, &n8);
            uvfend_ (&n8) ;
            }
         else
            (void)fprintf (stderr, "%s\n", msgbuf);
         return(1);
         }
      }
   bcopy ((char *)data, (char *)gainsdata, (int)head.datalen);

   return(0);
}

printStatusError(rtn)
char *rtn;
{
   char buf[256];

   if (status) {
      fcgerr (status, buf);
      (void)sprintf (msgbuf,
         "%s status %d: %s", rtn, status);
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)sprintf (msgbuf, "%s", buf);
      if (ierr == 0) {
         zmsgwr_ (msgbuf, &n8);
         uvfend_ (&n8) ;
         }
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)exit(3);
      }
}


goWriteFitsHeader(name)
char *name;
{
   char buf[256], soname[20], cdate[20], observ[20];
   double dateut;
   float temp[10];
   int naxes[10], ida, imo, iyr;
   static char *cstr[] = { "COMPLEX ", "STOKES  ", "FREQ    ",
      "RA      ", "DEC     " };
  static char *pstr[] = { "RA      ", "DEC     ", "DATE    ",
      "DATE    ", "BEAM    ", "SCAN    ", "SAMPLE  "};

   (void)sprintf (buf, "rm -f %s", name);
   (void)system (buf);
   (void)sprintf (msgbuf, "Initializing FITS header for %s\n", name);
   if (ierr == 0)
      zmsgwr_ (msgbuf, &n4);
   else
      (void)printf("%s\n", msgbuf);

                                         /* initialize some variables */
   (void)strncpy (soname, scanhead.object, 16);
   soname[16] = '\0';
   (void)strncpy(observ, scanhead.obsid, 16);
   observ[16] = '\0';
   dateut = (double)scanhead.utdate + 0.000001;    /* float round off */
                                          /* Open the new UVFITS file */
   fcinit (iunit, name, 2880, &status);
   printStatusError ("fcinit");
                                           /* Setup default header... */
   naxes[0] = 0;
   naxes[1] = 3;
   naxes[2] = 1;
   naxes[3] = noint;
   naxes[4] = 1;
   naxes[5] = 1;
   gcount = 1;
                         /* Write the required primary array keywords */
   fcphpr (iunit, 1, -32, 6, naxes, 7, gcount, 1, &status);
   printStatusError ("fcphdr");
                             /* Write other primary array keywords... */
   fcpkys (iunit, "OBJECT", soname, "", &status);
   printStatusError ("fcpkys");
   fcpkys (iunit, "TELESCOP", "NRAO 12m", "", &status);
   printStatusError ("fcpkys");
   fcpkys (iunit, "OBSERVER", observ, "", &status);
   printStatusError ("fcpkys");
   iyr = (int)(dateut)-1900;
   imo = (int)(100.0*(dateut-(double)(iyr+1900)));
   ida = (int)(100.0*(100.0*(dateut-(double)(iyr+1900))-(double)(imo)));
   (void)sprintf (cdate,"%2.2d/%2.2d/%2.2d        ", ida, imo, iyr);

   fcpkys (iunit, "DATE-OBS", cdate, "", &status);
   printStatusError ("fcpkys");
   fcpkys (iunit, "BUNIT", "K", "NOTE...scale is TR*", &status);
   printStatusError ("fcpkys");
   fcpkye (iunit, "EPOCH", scanhead.epoch, 9, "epoch of RA DEC",
      &status);
   printStatusError ("fcpkye");
   fcpkyj (iunit, "VELREF", 257, ">256=radio, 1=lsr, 2=hel, 3=obs",
      &status);
   printStatusError ("fcpkyj");
                               /* X0 needs to be in m/sec for FITS... */
   fcpkye (iunit, "ALTRVAL", scanhead.x0*1.0e3, 9,
      "velocity reference value", &status);
   printStatusError ("fcpkye");
   fcpkye (iunit, "ALTRPIX", scanhead.refpt, 9,
      "velocity reference pixel", &status);
   printStatusError ("fcpkye");
   fcpkye(iunit, "OBSRA", scanhead.xsource, 9, "RA reference position",
      &status);
   printStatusError ("fcpkye");
   fcpkye (iunit, "OBSDEC", scanhead.ysource, 9,
      "Dec reference position", &status);
   printStatusError ("fcpkye");
                              /* RESTFR needs to be in Hz for FITS... */
   fcpkye (iunit, "RESTFREQ", scanhead.restfreq*1.0e6, 9,
     "rest frequency", &status);
   printStatusError ("fcpkye");

                      /* NOTE...I have to reset naxes(0) to 1 because */
                      /* of a quirk in the FITSIO routines...  */
   naxes[0] = 1;
   fcpdef (iunit, -32, 6, naxes, 7, gcount, &status);
   printStatusError ("fcpdef");
                                      /* Write the CTYPE* keywords... */
   fcpkns (iunit, "CTYPE", 2, 5, cstr, "", &status);
   printStatusError ("fcpkns");
                                          /* Write CRVAL* keywords... */
   temp[0] = 1.0;
   temp[1] = -1.0;
   temp[2] = scanhead.restfreq*1.0e6;  /* freqr in MHz, FITS wants Hz.*/
   temp[3] = 0.0;
   temp[4] = 0.0;
   fcpkne (iunit, "CRVAL", 2, 5, temp, 9, "", &status);
   printStatusError ("fcpkne");
                                          /* Write CDELT* keywords... */
   temp[0] = temp[3] = temp[4] = 1.0;
   temp[1] = -1.0;
   temp[2] = scanhead.freqres*1.0e6;   /* freqr in MHz, FITS wants Hz */
   fcpkne(iunit, "CDELT", 2, 5, temp, 9, "", &status);
   printStatusError ("fcpkne");
                                          /* Write CRPIX* keywords... */
   temp[0] = temp[1] = temp[3] = temp[4] = 1.0;
   temp[2] = scanhead.refpt;
   fcpkne (iunit, "CRPIX", 2, 5, temp, 9, "", &status);
   printStatusError ("fcpkne");
                                          /* Write CROTA* keywords... */
   temp[0] = temp[1] = temp[2] = temp[3] = temp[4] = 0.0;
   fcpkne (iunit, "CROTA", 2, 5, temp, 9, "", &status);
   printStatusError ("fcpkne");
                                          /* Write PTYPE* keywords... */
   fcpkns (iunit, "PTYPE", 1, 7, pstr, "", &status);
   printStatusError ("fcpkns");
                                         /* Write null PSCALE* values */
   temp[0] = temp[1] = temp[2] = temp[3] = temp[4] = temp[5] = 1.0;
   temp[6] = 1.0;
   fcpkne (iunit, "PSCAL", 1, 7, temp, 9, "", &status);
   printStatusError ("fcpkne");
                                        /* Write PZERO* values... */
   temp[0] = temp[1] = temp[2] = temp[3] = 0.0;
   temp[4] = temp[5] = temp[6] = 0.0;
   fcpkne (iunit, "PZERO", 1, 7, temp, 9, "", &status);
   printStatusError ("fcpkne");
}


goWriteFitsData()
{
   static int k=1;
   int i, ndat, juld, nrec, ida, imo, iyr, dataindx;
   float *utStart, *raStart, *decStart, weight,
      gparms[7], dateut, rjuld, rjult;
   struct UVDAT *p;

      /* Calculate the Julian date and time from the UTDATE and UTTIME.
                   Remember that the Julian Day starts at noon UT...  */
   dateut    = onhead.utdate;
   iyr       = (int)(dateut);
   imo       = (int)(100.0*(dateut-(float)(iyr)));
   ida       = (int)(100.0*(100.0*(dateut-(float)(iyr))-(float)(imo)));
   juld      = julday(imo, ida, iyr);
   weight    = onhead.stsys / 1000.0 ;
   if (weight <= 0.0) weight = 1.0;
   if (onhead.samprat > 0)
      weight = (10.0 * onhead.samprat) / (weight * weight) ;
   else
      weight = (1.0) / (weight * weight) ;
   gparms[5] = onhead.scan;
   nrec      = (int)(scanhead.inttime / scanhead.samprat);
   utStart   = &ondata[nrec * noint];
   raStart   = utStart + nrec;
   decStart  = raStart + nrec;
   dataindx  = 0;

   for (p=uvdat; p<uvdat+noint; p++)
      p->weight = weight;

                                       /* for each 100ms integrations */
   for (i=0; i<nrec; i++, gcount++) {
                                /* UTTIME is in milliseconds, which   */
                                /* needs to be converted to a "Julian */
                                /* time" in hours at Greenwich mean   */
                                /* noon (1200 UT)...                  */
      rjult = (*utStart++ / (1.0e3*3600.0)) + 12.0;
                                /* Put group parameters into GPARMS   */
                                /* XSOURCE, YSOURCE from SDD in asec  */
                                /* converted to degrees for FITS      */
      gparms[0] = scanhead.xsource + *raStart++;
      gparms[1] = scanhead.ysource + *decStart++;
      gparms[2] = (float)(juld);
      gparms[3] = rjult / 24.0;
      gparms[4] = k+256;
      gparms[6] = k;
      fcpgpe (iunit, gcount, 1, 7, gparms, &status);
      printStatusError ("fcpgpe");

     /* Now transfer the real, offset, and weight values to the output
            UV data array.  If a channel value has been flagged by the
                  on-line system, then assign it the BLANK value...  */
      for (p=uvdat, ndat=0; p<uvdat+noint; p++, dataindx++, ndat++) {
         p->data = ((ondata[dataindx] -offdata[ndat])
            / offdata[ndat]) * gainsdata[ndat];
         if (ondata[dataindx] == 1.0e-20) p->data = -32768.0;
         }

                             /* Write the primary array of UV data... */
      fcppre(iunit, gcount, 1, 3*noint, uvdat, &status);
      printStatusError ("fcppre");
      k++;
      }
}


/* Now insert the proper value for GCOUNT (number of visibilities),
   BSCALE, BZERO, and BLANK... and close fits file.  */

goWriteFitsTail()
{
   double bscale, bZero, blank;

   bscale = 1.0;
   bZero = 0.0;
   blank = -32768.0;
   gcount--;
   fcmkyj (iunit, "GCOUNT", gcount,"num of rows * num of samples",
      &status);
   printStatusError ("fcmkyj");
   fcpkyd (iunit, "BSCALE", bscale, 9, "", &status);
   printStatusError ("fcpkyd");
   fcpkyd (iunit, "BZERO", bZero, 9, "", &status);
   printStatusError ("fcpkyd");
   fcpkyd (iunit, "BLANK", blank, 9, "", &status);
   printStatusError ("fcpkyd");
   fcpcom (iunit, "Written by uvf by Jeff Mangum and Tom Folkers",
      &status);
   printStatusError ("fcpcom");
                                        /* Now close the table... */
   fcclos (iunit, &status);
   printStatusError ("fcclos");
}
/*  function JULDAY
  This function returns the Julian Day Number which begins at noon UT
  of the calendar date specified by month (IM), day (ID), and year (IY).
  Positive year signifies A.D.; negative B.C.  Remember that the year
  after 1 B.C. was 1 A.D.
  Gregorian calendar was adopted on October 15, 1582...  */

julday (im, id, iy)
int im, id, iy;
{
  int jm, jy, ja, jd;

   if (iy == 0) {
      (void)sprintf (msgbuf, "There is no year zero.");
      if (ierr == 0) {
         zmsgwr_ (msgbuf, &n8);
         uvfend_ (&n8) ;
         }
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)exit(3);
      }
   if (iy < 0)
      iy++;

   if (im > 2) {
      jy = iy;
      jm = im + 1;
      }
   else {
      jy = iy - 1;
      jm = im + 13;
      }
   jd = (int)(365.25 * (float)jy) + (int)(30.6001 * (float)jm) + id
      + 1720995;
   if (( id + 31 * (im + 12 * iy)) > GREG) {
      ja = (int)(0.01 * (float)jy);
      jd = jd + 2 - ja + (int)(0.25 * ja);
      }
   return (jd);
}

read_scan (scan, filename )
float scan;
char *filename;
{
   int fd, dirl, bytesinindex, bytesused, num, max;
   int i,l, sizeofDIR;
   int datalen;
   struct BOOTSTRAP  boot;
   struct DIRECTORY  dir;

   sizeofDIR = sizeof(struct DIRECTORY);
   if (data) {
      free((char *)data);
      data = NULL;
      }

   if ((fd = pdopen (filename)) < 0)
      return(-1);

   if (read (fd, (char *)&boot, sizeof(struct BOOTSTRAP)) < 0) {
      perror ("read boot");
      if (ierr == 0) {
         sprintf (msgbuf, "Read error on boot area of %s", filename);
         zmsgwr_ (msgbuf, &n8);
         }
      (void)close (fd);
      return (-1);
      }

                                             /* check bootstrap block */
   if (sizeofDIR != boot.bytperent ) {
      (void)sprintf (msgbuf, "directory sizes dont match");
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)close(fd);
      return(-1);
      }

   bytesinindex = boot.num_index_rec * boot.bytperrec;
   bytesused = boot.num_entries_used * boot.bytperent;

   if (bytesinindex < bytesused + sizeofDIR) {
      (void)sprintf (msgbuf, "Directory structure of %s is full.",
         filename );
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)close (fd);
      return (-1);
      }

   if (boot.typesdd || !boot.version) {
      (void)sprintf (msgbuf, "SDD version or type of %s is invalid.",
         filename);
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)close (fd);
      return (-1);
      }

   if (sizeofDIR != boot.bytperent) {
     (void)sprintf (msgbuf, "File %s has faulty directory size",
        filename);
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)close (fd);
      return (-1);
      }

   if (boot.num_entries_used <= 0) {
     (void)sprintf (msgbuf, "File %s is empty", filename);
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      (void)close (fd);
      return (-1);
      }

   if (scan < 0.0) {
      dirl = (boot.num_entries_used-1) * boot.bytperent
         + boot.bytperrec;
      (void)lseek (fd, (off_t)dirl, SEEK_SET);
      if (read (fd, (char *)&dir, sizeofDIR) != sizeofDIR) {
         perror ("readdir");
         if (ierr == 0) {
            sprintf (msgbuf, "Read error on directory of %s", filename);
            zmsgwr_ (msgbuf, &n8);
            }
         (void)close (fd);
         return (-1);
         }
      }
   else if (scan > 0.0) {
      num = 0;
      while (num < boot.num_entries_used) {
         dirl = num * boot.bytperent + boot.bytperrec;
         (void)lseek (fd, (off_t)dirl, SEEK_SET);
         if (read (fd, (char *)&dir, sizeofDIR ) != sizeofDIR) {
            perror ("readdir");
            if (ierr == 0) {
               sprintf (msgbuf, "Read error on directory of %s",
                  filename);
               zmsgwr_ (msgbuf, &n8);
               }
            (void)close (fd);
            return (-1);
            }
         if (scan == dir.scan)
            break;
         num++;
         }
      if (num >= boot.num_entries_used) {
         (void)sprintf (msgbuf, "Scan %7.2f not found", scan);
         if (ierr == 0)
            zmsgwr_ (msgbuf, &n6);
         else
            (void)fprintf (stderr, "%s\n", msgbuf);
         (void)close (fd);
         return (-1);
         }
      }

   max = boot.bytperrec * (dir.start_rec - 1);
   (void)lseek (fd, (off_t)max, SEEK_SET);
   if (read (fd, (char *)&head, sizeof(struct HEADER)) < 0) {
      perror ("read header");
      if (ierr == 0) {
         sprintf (msgbuf, "Read error on header on %s", filename);
         zmsgwr_ (msgbuf, &n8);
         }
      (void)close (fd);
      return (-1);
      }

   if (scan > 0.0) {                                /* read data recs */
      datalen = (int)head.datalen;
      data = (float *)malloc ((unsigned)datalen);
      bzero ((char *)data, datalen);

      if (read (fd, (char *)data, datalen) < 0) {
         perror ("read data");
         (void)sprintf (msgbuf, "Error reading scan %7.2f data", scan);
         if (ierr == 0)
            zmsgwr_ (msgbuf, &n8);
         else
            (void)fprintf (stderr, "%s\n", msgbuf);
         (void)close (fd);
         return (-1);
         }

                        /* now check for abnormal float in data array */
      l = datalen / sizeof(float);
      for (i = 0; i < l; i++)
         if (!isnormal ((double)data[i]))
            data[i] = 0.0;
      }

   (void)close (fd);
   return ((int)head.scan);
}

static pdopen (name)
char *name;
{
   int fd;

   if ((fd = open (name, O_RDONLY, 0666 )) < 0) {
      extern int errno, sys_nerr;
      extern char *sys_errlist[];

      if (errno > sys_nerr)
         (void)sprintf (msgbuf, "%s open error number %d\n", name,
            errno );
      else
         (void)sprintf (msgbuf, "%s open: %s\n", name,
            sys_errlist[errno] );
      if (ierr == 0)
         zmsgwr_ (msgbuf, &n8);
      else
         (void)fprintf (stderr, "%s\n", msgbuf);
      return (-1);
      }

   return (fd);
}
