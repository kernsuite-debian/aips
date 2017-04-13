LOCAL INCLUDE 'RPFITS.INC'
C---------------------- COMMONS FOR RPFITS ROUTINES --------------------
C
C                SEE RPFITS.DEFN FOR an EXPLANATION
C
C        Modifications:
C        rpn 7/11/88     added if table and if common
C        rpn 8/11/88     inserted an table and extended antenna common
C                        to include polarisation
C        rpn 9/11/88     major change in treatment of if's. For multi-if
C                        data, rpfits should be called once per if (i.e.
C                        several times per integration), with a formal
C                        parameter if_no varying from 1 to n_if.
C                        A new group will be written for each if.
C                        PTI data will continue to be written with
C                        NSTOK = 2.
C        rpn 9/11/88     added su and fg tables
C        rpn 8/2/89      dates changed from AEST to UT
C        rpn 10/2/89     changed INTEGER*4 declaration to INTEGER for
C                        AIPS.
C        rpn 17/2/89     Put in INDEX common
C        rpn 24/5/89     Put in VERSION string
C        rpn 27/7/89     Put in if_sampl, ant_mount, changed names of
C                        pressure etc to ant_...
C        rpn 10/10/89    put in su_found, if_found, etc.
C        rpn 11/10/89    put in MT commons
C        rpn 8/11/89     put in longer strings for first 4 variables in
C                        /NAMES/
C        rpn 8/11/89     put in su_rad, su_decd
C        rpn 20/3/90     put in su_num, if_num, and changed ant_no to
C                        ant_num.  Added sc common.  Put in write_wt,
C                        if_ref.
C        rpn 22/3/90     put in CU common
C        hm 11/5/90      removed tabs and changed real*4 to real
C                        also cut lines down to 72 characters
C        hm 2/7/90       removed unused variables and changed real*8
C                        to double precision.
C        hm 14/11/91     Added if_sumul and if_chain arrays to if table
C                        - to handle sumiltaneous frequencies.
C        hm 11/3/92      Increased max_su from 16 to 500 to allow
C                        for mosaicing of up to 500 sources per scan.
C                        Allow for separate phase and pointing centres
C                        by adding new arrays for pointing centres -
C                        su_pra, su_pdec, su_prad, su_pdecd
C        hm 19/3/92      Added sc_srcno
C        hm 29/9/92      Increased max_card from 256 to 650
C        hm 23/6/93      Collected character variables into a single
C                        common.
C        hm 08/12/93     Added intbase
C        hm 10/03/94     Added proper motion keywords PMRA, PMDEC,
C                        PMEPOCH
C        hm 28/02/96     Change for multi-beam data. Data real not complex.
C                        Added data_format to be used by rpfitsout to
C                        determine how data is written - instead of
C                        write_wt.
C        hm 14/02/97     Increase max antennas to 15 for multibeam.
C        hm 02/06/98     VERSION 2 RPFITS. Change char date formats to
C                        YYYY-MM-DD stored in a 12 character string.
C        hm 04/11/1998   Version 2.1 RPFITS. Change VERSION to char*20
C                        and add rpfitsversion, also char*20
C        eg 17/10/2015   Added INTAPE to this common to get it where
C                        needed
C-----------------------------------------------------------------------
        integer ant_max, max_card, max_if, pol_max, max_su, max_fg,
     +        max_nx, max_mt, max_sc, max_cu
        parameter (ant_max=15, max_card=650, max_if=8, pol_max=8,
     +        max_su=500, max_fg=32, max_nx=256, max_mt=256,
     +        max_sc=16, max_cu=32)

        integer nstok, nfreq, ncount, nscan, ivelref, nant, ncard,
     +        INTAPE, intime, rp_defeat, NNIF, IFNUMS(max_if),
     +        if_invert(max_if), if_nfreq(max_if),
     +        if_nstok(max_if), if_sampl(max_if), if_simul(max_if),
     +        if_chain(max_if), ant_num(ant_max), NNSU, SUNUM(max_su),
     +        n_fg, fg_ant(2,max_fg),fg_if(2, max_fg),
     +        fg_chan(2, max_fg), fg_stok(2, max_fg), n_nx,
     +        nx_rec(max_nx), ant_mount(ant_max),
     +        rp_iostat, n_mt, mt_ant(max_mt),
     +        sc_ant, sc_if, sc_q, n_cu, cu_ant(max_cu),
     +        cu_if(max_cu), cu_ch1(max_cu), cu_ch2(max_cu),
     +        sc_srcno, data_format

      double precision ra, dec, freq, dfreq, rfreq, vel1, rp_utcmtai,
     +        rp_c(12), rp_djmrefp, rp_djmreft,x(ant_max),
     +        y(ant_max), z(ant_max), if_freq(max_if), x_array,
     +        y_array, z_array, axis_offset(ant_max),
     +        feed_pa(2,ant_max), feed_cal(ant_max, max_if, pol_max),
     +        if_bw(max_if), fg_ut(2, max_fg), su_ra(max_su),
     +        su_dec(max_su), su_rad(max_su), su_decd(max_su),
     +        if_ref(max_if), nx_ut(max_nx), mt_press(max_mt),
     +        mt_temp(max_mt), mt_humid(max_mt), mt_ut(max_mt),
     +        cu_ut(max_cu), cu_cal1(max_cu), cu_cal2(max_cu),
     +        su_pra(max_su), su_pdec(max_su), su_prad(max_su),
     +        su_pdecd(max_su), pm_ra, pm_dec, pm_epoch

      real sc_ut, sc_cal(max_sc,max_if, ant_max), intbase

      character*2 feed_type(2,ant_max), if_cstok(4,max_if)
      character*16 object,instrument,cal,rp_observer
      character*8 sta(ant_max), coord, datsys
      character*20 version, rpfitsversion
      character*12 datobs, datwrit
      character*256 file
      character*48 aifile
      character*80 card(max_card)
      character su_name(max_su)*16, su_cal(max_su)*4,
     +       fg_reason(max_fg)*24, nx_date(max_nx)*12,
     +       nx_source(max_nx)*16
      logical if_found, su_found, fg_found, nx_found, an_found,
     +       mt_found, cu_found, write_wt

      common /doubles/ axis_offset, dec, dfreq, cu_cal1, cu_cal2,
     +      cu_ut, feed_cal, feed_pa, fg_ut, freq, if_bw, if_ref,
     +      if_freq, mt_humid, mt_press, mt_temp, mt_ut, nx_ut,
     +      ra, rfreq, rp_c, rp_djmrefp, rp_djmreft, rp_utcmtai,
     +      su_dec, su_ra, su_rad, su_decd, su_pra, su_pdec,
     +      su_prad, su_pdecd, vel1, x, x_array, y, y_array, z,
     +      z_array
      common /proper/ pm_ra, pm_dec, pm_epoch
      common /param/ nstok, nfreq, ncount, intime, nscan, write_wt,
     +       ncard, intbase, data_format, INTAPE
      common /spect/ ivelref
      common /anten/ nant, ant_num, ant_mount, an_found
      common /ephem/ rp_defeat
      common /if/ NNIF, if_invert, if_nfreq, if_nstok,
     +        if_sampl, if_found, IFNUMS, if_simul, if_chain
      common /su/ NNSU, su_found, SUNUM
      common /fg/ n_fg, fg_ant, fg_if, fg_chan, fg_stok, fg_found
      common /nx/ n_nx, nx_rec, nx_found
      common /mt/ n_mt, mt_ant, mt_found
      common /ATindex/ rp_iostat
      common /sc/ sc_ut, sc_ant, sc_if, sc_q, sc_cal, sc_srcno
      common /cu/ n_cu, cu_ant, cu_if, cu_ch1, cu_ch2, cu_found
      common /names/ object, instrument, cal, rp_observer, datobs,
     +       datwrit, file, datsys, version, coord, sta, feed_type,
     +       card, if_cstok, su_name, su_cal, fg_reason, nx_source,
     +       nx_date, rpfitsversion, aifile


C     the following is for compatibility with early versions:
      double precision rp_pressure(ant_max), rp_temp(ant_max),
     +    rp_humid(ant_max), ant_pressure(ant_max),
     +    ant_temp(ant_max), ant_humid(ant_max)
      equivalence ( rp_pressure(1), ant_pressure(1))
      equivalence ( rp_temp(1), ant_temp(1))
      equivalence ( rp_humid(1), ant_humid(1))
      equivalence ( mt_press(1), ant_pressure(1))
      equivalence ( mt_temp(1), ant_temp(1))
      equivalence ( mt_humid(1), ant_humid(1))
LOCAL END
LOCAL INCLUDE 'ATLOD.INC'
C                                       Local common for ATLOD

C         >>>>>>>>   HEY! DON'T FORGET TO DO THIS  <<<<<<<<
C                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C     Update the RELEASE NUMBER and DATE whenever you modify ATLOD.
      CHARACTER ATLODD*11, ATLODV*4
C     PARAMETER (ATLODD = '1995/Nov/10', ATLODV = '4.4')
C     PARAMETER (ATLODD = '1997/Sep/05', ATLODV = '4.5')
C     PARAMETER (ATLODD = '1998/Jan/04', ATLODV = '4.6')
C     PARAMETER (ATLODD = '1998/Jan/17', ATLODV = '4.62')
C     PARAMETER (ATLODD = '1998/Feb/10', ATLODV = '4.64')
C     PARAMETER (ATLODD = '1998/Feb/11', ATLODV = '4.65')
C     PARAMETER (ATLODD = '1998/Feb/18', ATLODV = '4.66')
C     PARAMETER (ATLODD = '1998/Mar/04', ATLODV = '4.7')
C     DRL carefully merged JER ATLOD version 4.73 into
C     RGD ATLOD version 4.7 to become ATLOD version 4.75
C     PARAMETER (ATLODD = '1998/Nov/08', ATLODV = '4.75')
C  Increase MAXDAT to 16400 - JER
C     PARAMETER (ATLODD = '1998/Dec/21', ATLODV = '4.76')
C  ATNF standard version, 15APR98 AIPS (HMay)
C     PARAMETER (ATLODD = '1998/Sep/02', ATLODV = '5.1')
C  Increase NCA to 8 stations, check NCA exceeded
C     PARAMETER (ATLODD = '1999/Jun/01', ATLODV = '5.2')
C  Fix date for EOP file (using AIPS nastinesses).
C  Bug fix, restrict XYZ=0 -> flagged only for ATCA data.
C     PARAMETER (ATLODD = '2000/Jan/26', ATLODV = '5.3')
C  Remove multiple declarations (DATOB8, DATOBS) for Linux
C     PARAMETER (ATLODD = '2000/May/25', ATLODV = '5.3b')
C  Incease MAXDAT size from 16400 to 8193ch * 4pol = 32772
C     PARAMETER (ATLODD = '2000/Aug/21', ATLODV = '5.3c')
C  Catch invalid EPOCH values e.g. EPOCH = 'AZEL')
C     PARAMETER (ATLODD = '2001/Jun/26', ATLODV = '5.3d')
C  Check for invalid ANT_NUM values in AN TABLE (c.f. LBA bug 1996
C     and earlier with antenna number = 0 in AN tables).
C     PARAMETER (ATLODD = '2001/Oct/10', ATLODV = '5.3e')
C  Minor bug: increase MAXREG to 14!
C      PARAMETER (ATLODD = '2002/Feb/15', ATLODV = '5.3f')
C  Major bug: Typo forced incorect UVW calculation
C      PARAMETER (ATLODD = '2007/Feb/20', ATLODV = '5.3g')
C Increase IF parameters
C      PARAMETER (ATLODD = '2008/Jul/28', ATLODV = '5.3h')
C Remove dependency on atelib/slalib
C      PARAMETER (ATLODD = '2008/Nov/01', ATLODV = '5.3i')
C Bugfix release
C      PARAMETER (ATLODD = '2008/Dec/10', ATLODV = '5.3j')
C XY-Mount support and large file overflow
      PARAMETER (ATLODD = '2008/Dec/10', ATLODV = '5.3k')

C     Dates when AT conventions changed
      CHARACTER*8 DATBAS, DATTSY, DATIN1, DATIN2, DATSS1, DATSS2,
     *            DATSSC, DATSRC, DATUVW

C     Invert switch stuck at +1 until 19/06/90 when it got stuck
C     at -1.  Unstuck on 01/08/90.  Not currently used by ATLOD
      PARAMETER (DATIN1 = '19/06/90', DATIN2 = '01/08/90')

C     Baseline naming convention changed from 2-1 to 1-2 on 09/07/90
      PARAMETER (DATBAS = '09/07/90')

C     Off line Tsys correction possible from 13/08/90
      PARAMETER (DATTSY = '13/08/90')

C     Sampler statistics 17.1% in all correlator configurations
C     after DATSS1, 17.3% after DATSS2.  Don't cope for earlier data.
      PARAMETER (DATSS1 = '20/06/91', DATSS2 = '21/08/93')

C     Sampler statistics corrections done in correlator after
C     this date
      PARAMETER (DATSSC = '11/12/93')

C     After this date, data have the SC_SRCNO variable set for SYSCAL
C     data.
      PARAMETER (DATSRC = '09/04/92')

C     LBA data after this date have correct UVWs in the RPF file.
      PARAMETER (DATUVW = '20/02/98')

C     Convert radians to degrees
      DOUBLE PRECISION R2D
      PARAMETER (R2D = 180.0D0/3.141592653589793D0)

C     Frequency match tolerance with user specified frequency
      DOUBLE PRECISION FRQTOL
      PARAMETER (FRQTOL = 1.0D3)

C     Maximum number of antennas and baselines.
      INTEGER   NCA
      PARAMETER (NCA = 30)

C     Number of syscal parameters in the SYSNUM array.
      INTEGER   NSYSCP
      PARAMETER (NSYSCP = (NCA*(NCA+1))/2)

C     Declarations for Stokes conversion and Tsys correction.
C     Maximum stack sizes for the system calibration parameters:
C     XY phase (MAXPST), Tsys_X and Tsys_Y (MAXTST), for each FREQID
C     (MAXSFQ), each simultaneous frequency in the FREQID (MAXSSF),
C     antenna (NCA), and source (MAXSSU).
      INTEGER   MAXSSF, MAXSFQ, MAXPST, MAXTST, MAXSSU
      PARAMETER (MAXSSF = 16, MAXSFQ = 15, MAXSSU = 400, MAXPST = 10,
     *           MAXTST = 15)

      INTEGER   NSYSPH(NCA,MAXSSF,MAXSFQ),
     *          NSYSTX(NCA,MAXSSF,MAXSFQ,MAXSSU),
     *          NSYSTY(NCA,MAXSSF,MAXSFQ,MAXSSU),
     *          NSTAKP

      REAL      PHSTAK(MAXPST,NCA,MAXSSF,MAXSFQ),
     *          TXSTAK(MAXTST,NCA,MAXSSF,MAXSFQ,MAXSSU),
     *          TYSTAK(MAXTST,NCA,MAXSSF,MAXSFQ,MAXSSU),
     *          TXAVE(NCA,MAXSSF,MAXSFQ,MAXSSU),
     *          TYAVE(NCA,MAXSSF,MAXSFQ,MAXSSU),
     *          PHSAVE(NCA,MAXSSF,MAXSFQ), SSEXP, SSTOL
      LOGICAL CLIPPD(NCA,MAXSSF,MAXSFQ)



C     Visibility data buffers.  MAXDAT is big enough for one visibility
C     of 16384 channels (e.g. 8192 channels & 2 polarizations).
C     [To get 8193ch x 4 pols, need 32772: - JER]
C     Realistically, in simultaneous frequency (SF) mode, 8192 must
C     still be divided up amongst the SFs.  MAXACC is thus big enough
C     for 16384 simultaneous channels & 21 baselines to be accumulated
C     in one integration.   The maximum buffer size that the AIPS UV
C     I/O system can deal with is 32767 words, so the accumulation
C     buffer has to be copied to another buffer for output, one
C     visibility at a time.  1024 extra words to allow for fractional
C     sectors left over from  the previous write and for AIPS double
C     buffering machinations.
C
C     The standard AIPS buffer sizes can now handle this allocation.
C
      INTEGER MAXDAT, MAXACC, MAXOUT
      PARAMETER (MAXDAT = 8193*16, MAXACC = (MAXDAT+7)*(15+6)*3,
     *           MAXOUT = MAXDAT*3+9+1024)
      COMPLEX   VIS(MAXDAT), VIS2(MAXDAT)
      REAL WEIGHT(MAXDAT), WCHNSL(MAXDAT), UVBUFF(MAXACC),
     *     UVOUT(MAXOUT), AVGWT, NWT

C     Conversion for baselines counter to baseline number and vice versa
      INTEGER IBASE, BASIND(256*NCA+NCA), INDBAS(NCA*(NCA-1)/2+NCA)
      LOGICAL RANDUN(NCA*(NCA-1)/2+NCA)
      INTEGER BASAX(NCA*(NCA-1)/2+NCA,MAXSSF)


C     Summarize at end of file the severe errors
      INTEGER MAXSEV, NSEVER
      PARAMETER (MAXSEV = 1000)
      CHARACTER*80 SEVERE(MAXSEV)
C
      LOGICAL   ANTIN(NCA), DOAUTO, DOHIST, DOLIST, DONPHA, DORRLL,
     *          DOSNEG, DOSTOK, DOSWNT, DOUVCM, DOXYFL, DROFLG,
     *          SCANTN(NCA), SHADED(NCA), TBSORT, USERXY, YGNROT,
     *          SYSDMP, CHKPNT, EXBSLN(NCA,NCA), IFMAP, TSYFDG, HANN,
     *          FLGFDG, REDOUV
      INTEGER   BASELN, BCHAN, ECHAN, BIN, BSCAN, CNO, ESCAN,
     *          FILSEQ, FLAG, HIBUFF(512), HILUN, SSCHK, LREC2,
     *          IFNUM, LISLEV, LREC, NAXVAL, NBLKS, NFILES,
     *          NIO, NPIO, NSCANS, NSKIP, NSOURC, NUVOPN, OUTDSK,
     *          OUTSEQ, PREVFQ, PREVSU, PRTSYS, SRCNUM, UVBIND, NAXVL2,
     *          UVFIND, UVLUN, VISCNT, VSCAN1, VSCAN2, OVFLOW, OVFLIM,
     *          CHANSL(3,10), NCHNSL, DOTSYS, TRUPOL, NCHAN, NFREQS,
     *          NUMIF, NIFS, IFS(5), PBIN1, PBIN2, PBIN3, RCMPUV
C    *          , FLAG_IF1
Crgd >
      real      dm,period
      integer   nbins
Crgd <

      REAL      BASLEN(NCA,NCA), BASMIN, CLSPAN, NXGAP, NXSPAN, PHSCLP,
     *          SHADOW, TIMRNG(2), U, UT, V, W, XAPARM(10), XBPARM(10),
     *          XBSCAN, XCPARM(10), XDOUVC, XDPARM(10), XITAPE, XNFILE,
     *          XNSCAN, XNSKIP, XOUTDI, XOUTSQ, XSELFR(30), XTIMER(8),
     *          XCHNSL(3,10), XYPHAS(6,6), XNIF, XIFMAP,
     *          XSELIFN(5)
      DOUBLE PRECISION FREQS(30), TSCAN1, TSCAN2
      DOUBLE PRECISION JULBAS, JULSYS, REFREQ, JULSS1, JULSS2, JULSSC,
     *                 JULSRC, JULUVW
      CHARACTER FILCLS*6, FILNAM*12, INFILE*48, OUTCLS*6, OUTNAM*12,
     *          SOURCS(30)*16, UVNAME*48
      CHARACTER DATOB8*8, DATWR8*8
      HOLLERITH XINFIL(12), XOPTYP, XOUTCL(2), XOUTNM(3), XSOURC(4,30)


C     Input adverbs.
      COMMON /INPARM/ XITAPE, XNSKIP, XNFILE, XBSCAN, XNSCAN, XINFIL,
     *                XOUTNM, XOUTCL, XOUTSQ, XOUTDI, XOPTYP, XSOURC,
     *                XTIMER, XSELFR, XSELIFN,XCHNSL, XIFMAP, XNIF,
     *                XDOUVC, XAPARM, XBPARM, XCPARM, XYPHAS, XDPARM
      COMMON /ATCTRL/ nbins, NSKIP,  NFILES, BSCAN,  ESCAN,  NSCANS,
     *                OUTSEQ, FILSEQ, OUTDSK, DOLIST, LISLEV, NSOURC,
     *                DOSWNT, TIMRNG, BCHAN,  ECHAN, DOUVCM, DOSTOK,
     *                DORRLL, DROFLG, DOAUTO, SHADOW, NXGAP,  NXSPAN,
     *                CLSPAN, DOTSYS, NSTAKP, DOXYFL, EXBSLN, PRTSYS,
     *                DOSNEG, DONPHA, WCHNSL, OVFLOW, OVFLIM, CHANSL,
     *                NCHNSL, USERXY, YGNROT, SYSDMP, CLIPPD, CHKPNT,
     *                NCHAN,  TRUPOL, NFREQS, FREQS,  NUMIF,  IFMAP,
     *                NSEVER, TSYFDG, HANN,   NIFS,   IFS,    FLGFDG,
     *                PBIN1,  PBIN2,  PBIN3,  REDOUV, RCMPUV
Crgd >
     *                ,dm,period
Crgd <

      COMMON /CHRCOM/ INFILE, OUTNAM, OUTCLS, FILNAM, FILCLS, SOURCS,
     *                SEVERE

C     Output UV file.
      COMMON /ATUVIO/ NUVOPN, CNO, NBLKS, UVLUN, UVFIND, LREC, NPIO,
     *                UVBUFF, UVBIND, NIO, VISCNT, NAXVAL, UVOUT,
     *                TBSORT, REFREQ, HILUN, DOHIST, HIBUFF, LREC2,
     *                NAXVL2
      COMMON /ATUVCH/ UVNAME

C     Scan parameters outside the scope of the RPFITS commons.
      COMMON /ATSCAN/ ANTIN, PREVFQ, PREVSU, TSCAN1, TSCAN2,
     *                VSCAN1, VSCAN2, SCANTN, BASIND, INDBAS,
     *                BASAX, IBASE, RANDUN


C     Visibility data.
      COMMON /VISBUF/ VIS, VIS2, U, V, W, BASELN, UT, SRCNUM, IFNUM,
     *   BIN, FLAG, WEIGHT, BASLEN, BASMIN, SHADED, AVGWT, NWT

C     System calibration
      COMMON /ATSYSC/ JULBAS, JULSYS, JULSRC, JULSS1, JULSS2, JULSSC,
     *                JULUVW,
     *                NSYSPH, NSYSTX, NSYSTY, PHSTAK, PHSAVE, TXSTAK,
     *                TXAVE, TYSTAK, TYAVE, PHSCLP, SSCHK, SSTOL, SSEXP


C     Source and frequency selection.  Depend on RPFITS includes. Note
C     that SELIF is declared much bigger that it really needs to be
C     (the total number of freqs. per scan is MAX_IF, not
C     MAX_IF*MAX_SIM)  This is just done for simplicity.  MAXSIM is the
C     maximum possible number of simultaneous frequencies available at
C     one time.
C
      INTEGER MAX_SIM
      PARAMETER (MAX_SIM = MAXSSF)
      INTEGER SELSRC(MAX_SU), SELAX(MAX_IF), SELNIF(MAX_IF),
     *  SELIF(MAX_IF,MAX_SIM), XYPHPT(MAXSSF,MAXSFQ), NSUSEL, NIFSEL,
     *  FQTAGS(MAX_IF), SUTAGS(MAX_SU,MAX_IF)
      COMMON /ATSEL /SELSRC, SELIF, SELNIF, SELAX, XYPHPT,
     *  NSUSEL, NIFSEL, FQTAGS, SUTAGS

C     Scan dates in AIPS local form yyyymmdd
      COMMON /DATES /DATOB8,DATWR8
LOCAL END
      PROGRAM ATLOD
C-----------------------------------------------------------------------
C! Task to read AT RPFITS format data
C# UV AT
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2010, 2012, 2015, 2017
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  Copyright (C) 1988-2009
C;  Australia Telescope National Facility, CSIRO, Epping, Australia
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C     ATLOD will load uv data from an RPFITS file into a multi-source
C     AIPS uv data set.  It creates the SU (SoUrce), FQ (FreQuency),
C     NX (scan iNdeX), and AN (ANtenna) type extension files.
C
C     Adverbs:
C  1        INTAPE     Input tape drive number
C  2        NSKIP      Number of tape files to skip
C  3        NFILES     Number of tape files to load
C  4        BCOUNT     First scan in file to load (same for all files)
C  5        NCOUNT     Number of scans to load (same for all files)
C  6-17     INFILE     Disk file containing RPFITS data (if any)
C  18-20    OUTNAME    Output uv name
C  21-22    OUTCLASS   Output uv class
C  23       OUTSEQ     Output sequence number
C  24       OUTDISK    Output disk number
C  25       OPTYPE     'LOAD' to load the data,
C                      'LIST' to summarize each scan,
C                      'SUMM' to summarize each file.
C                      'SYSC' to dump SYSCAL info to text files
C  26-145   SOURCES    Source selection list
C  146-153  TIMERANGE  Time range selection
C  154-183  FREQSEL    Frequency selections (MHz)
C  184-188  IFSEL      IF selection.
C  189-218  CHANSEL    Up to 10 channel-selection groups: begin,
C                      end,  and incr
C  219      IFMAP      Map IF chain to IF axis
C  220      NIF        Length of IF axis
C  221      DOUVCOMP   If true use compressed uv data format
C  222-231  APARM      Control parameters
C                      1) If +1 convert linear polarization to Stokes
C                         parameters.  If -1 the polarizations are
C                         called RR,LL,RL,LR regardless of their actual
C                         type and order.  If 0, no polarization fiddle
C                      2) If true retain flagged data
C                      3) If true, retain autocorrelations
C                      4) Shadowing diameter, 0 -> 22m.
C                      5) Maximum gap before starting a new scan index
C                         (NX) entry, min.
C                      6) Maximum length of a scan index (NX) entry, min
C                      7) Time span for calibration table (CL) entries,
C                         min.
C                      8) Number of integrations to acumulate for Tsys
C                         correction.
C                            0 => do nothing
C                           -1 => Undo online & replace with Tsys=50
C                            N => average over N integrations
C                      9) Number of integrations to acumulate for XY
C                         phase averaging
C                            0 => do no averaging
C                            N => average over N integrations
C                     10) If .NE. 0, do flagging based upon XY phases
C                         even if not converting to Stokes (as well as).
C                         If > 0 use inbuilt clipping tolerances,
C                         if < 0 clip with inbuilt tolerances AND
C                         if more than abs(APARM(10)) degrees
C                         from XY phase stack means
C  232-241  BPARM      Baselines to be rejected, specified as 10*I + J
C                      where I and J are the antenna numbers, order
C                      being irrelevant unless the value is negative
C                      If I is 0 all baselines containing antenna J will
C                      be dropped.  Examples
C                         12  Drop baselines 1-2 and 2-1
C                        -31  Drop baseline  3-1 (but not 1-3)
C                          4  Drop all baselines involving antenna 4
C  242-251  CPARM      More control parameters
C                      1) 1 => List system calibration information
C                         2 => Inform when drift detected in XY phase
C                              or Tsys and accumulators reset
C                         3 => 1 + 2
C                      2) If true, negate system calibration phases
C                      3) If true, negate the input visibility phases
C                      4) If true, use user given XYPHASE array rather
C                         than the on-line XY Phases whenever needed.
C                      5) If true, apply XYPHASE to Y gain, but don't
C                         do Stokes conversion.
C                      6) If 1, then report when sampler statistics are
C                         outside of 4% tolerances for an antenna
C                         If 2, then drop any visibilities involving
C                         antennas with poor statistics.
C                         If 3, then do 1 and 2
C                         If 4, correct for errors in sampler stats
C                      7) If CPARM(6) > 0, use CPARM(7) as percentage
C                         tolerance rather than inbuilt 4%
C                      8) If true, INFILE will be read as lower case.
C                         This is intended for reading from VAX disks
C                         mounted under unix via multinet.
C                      9) When OPTYPE='SYSC' then if
C                         A(9) = 1  Files are XY phase, Tx and Ty
C                              = 2  Files are X sampler stats
C                              = 3  Files are Y sampler stats
C                     10) If true, apply Tsys from IF axis 1 to axis 2
C                         This is a fudge for 2-frequency data in early
C                         1992
C  252-287  XYPHASE    Array of XY Phases, one for each antenna number
C                       in degrees and for up to 6 frequencies
C  288-297  DPARM      1)  Set all source numbers to 1.  Used for March
C                       1992 data where source numbers were scrambled
C                       folowwing initial mosacing tests.
C                      2) If > 0 then 3-point Hanning smooth spectra
C                      3) If > 0 the weights are set equal to DPARM(3)
C                        instead of the integration time in units of
C                        of 15 seconds
C
C     Called:
C          ATLOD:  {ATCLOS, ATFEOF, ATLIST, ATPARM, BLDNAM, GRAND,
C                   HEDHNT, READHD, RPOPEN, SCANIN, SKIPS, SETUV}
C          APLSUB: {DIE, MSGWRT}
C
C     Algorithm:
C          The main program is a driver for the principle routines.
C
C     Notes:
C       1) The tape adverbs (INTAPE, and NFILES) are ignored if a disk
C          file is specified.
C
C       2) Frequencies in AIPS.
C          Space has been provided in the AIPS catalogue header, FQ
C          table, CL table, and SU table for storing frequency
C          information as follows.
C
C          *) One FREQSEL random parameter value may be associated with
C             each visibility record.  This points to an entry in the
C             FQ (formerly CH) table containing the frequency offset in
C             (Hz), channel bandwidth (Hz), total bandwidth (Hz), and
C             sideband code (-1 = lower, +1 = upper) for each location
C             of the IF axis.
C
C          *) The regular part of the visibility data structure contains
C             a FREQ axis for storing spectral channel data.  The
C             nominal frequency used to calculate (u,v,w) in wavelengths
C             is that stored as the frequency of the reference pixel on
C             the FREQ axis.
C
C          *) The regular part of the visibility data structure also
C             contains an IF axis for multiple IFs within each observing
C             band.  Since in general the IFs may be irregularly spaced,
C             the IF axis simply indexes the IFs, and space has been
C             provided for defining them (max 15) in each entry of the
C             FQ table.   The IF axis is used for simultaneous data
C             at different frequencies.
C
C          *) The CL table contains space for a Doppler offset in each
C             IF as a function of time.
C
C          *) A column has been provided in the SU table for a source
C             specific frequency offset, for example, that applying to
C             a source recession velocity.  However, these entries only
C             apply in the absence of an FQ table, and may be considered
C             defunct.
C
C          *) The true frequency of an observation is the reference
C             frequency plus plus the peculiar source doppler offset
C             in the SU table, plus the IF frequency offset in the
C             FQ table, plus the doppler tracking offset in the CL
C             table.  Must be careful NOT to duplicate the FQ table
C             offset in the SU table offset.
C
C          Different IFs can have different channel and band widths.
C          However, the uv data structure cannot handle a situation
C          where different IFs have a different number of polarizations
C          or spectral channels.  In this case the data would have to be
C          split into different uv files.
C
C          In its full generality, AT data will be too complicated for
C          the AIPS uv data structure.  For example, a single
C          observation might consist of 1024 frequency channels in four
C          polarizations with a total bandwidth of 8 MHz at 6035
C          MHz, and 2048 channels with RR and LL polarizations and a
C          bandwidth of 4 MHz at 4885 MHz.  There is an upper limit of
C          of 8196 channels of data per integration period, but the IFs
C          may be stepped between integrations.
C
C       3) From the above it follows that AT data cannot in general be
C          stored in a single AIPS uv data file using the IF regular
C          axis and must be split between several, each without an IF
C          axis.
C
C          The FREQSEL random parameter is still required since the IFs
C          could be stepped in frequency between integration periods
C          without changing the number of polarization or spectral
C          channels.  ATLOD does not address these problems at the
C          moment.
C
C       4) Logical unit numbers used in ATLOD
C             TLUN  = 3, 10, 11, 12
C             UVLUN = 16
C             HILUN = 27
C             ANLUN = 28
C             SULUN = 41
C             FQLUN = 42
C             NXLUN = 43
C             CLLUN = 44
C
C       5) The call tree for ATLOD (excluding ordinary AIPS routines) is
C
C          ATLOD - ATPARM - strim
C                - BLDNAM
C                - TXTOPN
C                - TRMOPN
C                - SKIPF  - ATTELL
C                - RPOPEN - rpfitsin
C                - SKIPS  - READHD  - rpfitsin
C                                   - upcase
C                - HEDHNT - READHD  - rpfitsin
C                                   - upcase
C                         - IFMACH
C                         - SUMACH
C                - SETUV  - POLDEC
C                         - ATMSG
C                         - CHNPOL - ATMSG
C                         - ADJDES
C                         - SETSSC - ATMSG
C                         - FIXCHN
C                         - TAPHDR
C                         - ATHI
C                - SYSINI
C                - ATLIST - rpfitsin
C                         - TIMCON
C                         - RDTOCH
C                - TXTWRT
C                - ATEINI
C                - SCANIN - IFMACH
C                         - ATMSG
C                         - SUMACH
C                         - ATFQIN  - ATMSG
C                         - ATSUIN  - ATMSG
C                         - ATANT
C                         - ATUV    - ATMSG
C                                   - rpfitsin
C                                   - UVDUMP  - UVWCAL - ephmrs
C                                   - ATNXCL  - FIXSRT
C                                             - ATMSG
C                                   - FIXSRT
C                                   - TIMCON
C                                   - SYSLIS  - ATMSG
C                                   - SYSCIN  - SYSEXT
C                                             - DRIFT   - STATS
C                                             - ATMSG
C                                             - SYSFLG  - CLPDAT - STATS
C                                   - SHADIN
C                                   - SSCOR   - TWOBIT_GAIN_ADJUST --->
C                                       --> GAIN_PARAM     - GAUSS_LEVEL
C                                       --> TWOBIT_GAIN_R0 - G_QUAD
C                                   - TSYSCO
C                                   - CORAPP
C                                   - HSM
C                                   - LINSTK
C                                   - LINROT
C                                   - UVDUMP
C                                   - UVACUM
C                                   - CLPWRN  - ATMSG
C                         - REPORT  - ATMSG
C                                   - FIXIFL
C                - READHD - rpfitsin
C                         - upcase
C                - ATTELL
C                - GRAND  - GRAND
C                - ATFEOF - rpfitsin
C                - SEVREP
C                - ATCLOS
C                - TXTCLS
C
C          Subroutines which are contained in separate files are listed
C          in lower case.
C
C       6) To be done:
C          *) Process RPFITS flag tables.
C          *) Multiple output streams.
C          *) Implement subarrays?
C
C       7) As of version 4.5, support for pulsar bins added (JER)
C
C     Authors:
C          Mark Calabretta and Neil Killeen, Australia Telescope.
C          Origin:   November 1985
C
C-----------------------------------------------------------------------

      LOGICAL   EOF, FIRST, MORE, OPENED, DOTCLS, QUIT
      INTEGER   BUFF(256), IERR, IFILE, JERR, NHDR, TOTFLG,
     *          TOTVIS, TLUN(3), TFIND(3), NTXTR, NTXTW, TTY(2)

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'

      DATA FIRST /.TRUE./
      DATA TLUN /3, 10, 11/
      DATA NTXTR, NTXTW /0, 0/
      DATA TTY /5, 0/
C-----------------------------------------------------------------------

C     Initialize and get input parameters.
      CALL ATPARM (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR READING ATLOD ADVERBS.'
         CALL ATMSG (MSGTXT)
         GO TO 900
      END IF

C     Construct the input file name for tape or disk
      CALL BLDNAM (IERR)
      IF (IERR.NE.0) GO TO 900

C     Open text file for dumping of SYSCAL info
      IF (SYSDMP) THEN
         CALL TXTOPN (TLUN, TFIND, IERR)
         IF (IERR.NE.0) GO TO 900
      END IF

C     Open the terminal for interaction with user if listing SYSCAL info
      IF (PRTSYS.EQ.1 .OR. PRTSYS.EQ.3) THEN
         CALL TRMOPN (TTY, IERR)
         IF (IERR.NE.0) GO TO 900
         DOTCLS = .TRUE.
         CHKPNT = .TRUE.
      ELSE
         DOTCLS = .FALSE.
      END IF

C     Skip files.
      IF (INFILE.EQ.' ') THEN
         CALL SKIPF (INTAPE, NSKIP, IERR)
         IF (IERR.NE.0) GOTO 900
      ELSE
         NSKIP = 0
      END IF

C     Loop over the number of files to read.
      OVFLIM = 200
      IFILE = 1
 5    IF (IFILE.LE.NFILES) THEN
         TOTVIS = 0
         TOTFLG = 0
         EOF    = .FALSE.
         OPENED = .FALSE.
         OVFLOW = 0
         QUIT = .FALSE.
         AVGWT = 0.0
         NWT = 0.0

C        Open RPFITS file
         CALL RPOPEN (IERR)
         IF (IERR.NE.0) GO TO 900

         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         WRITE (MSGTXT, 10) IFILE+NSKIP
   10    FORMAT ('Processing file:',I3)
         CALL ATMSG (MSGTXT)

C        Skip requested scans.  Useful only for one file at a time.
         CALL SKIPS (BSCAN, NHDR, IERR)
         IF (IERR.NE.0) GO TO 900

C        Hunt for the first RPFITS header with which to build the
C        header for the output AIPS file
         CALL HEDHNT (IFILE+NSKIP, NHDR, EOF, IERR)
         IF (IERR.EQ.-1 .OR. EOF) THEN
            IERR = 0
            GOTO 30
         ELSE IF (IERR.NE.0) THEN
            GOTO 900
         END IF

C        Create AIPS files if loading the data to disk.
         IF (.NOT.DOLIST .AND. .NOT.SYSDMP) THEN
C           Create or reopen the UV file.
            CALL SETUV (IERR)
            IF (IERR.EQ.10) GO TO 30

            OPENED = .TRUE.
            IF (IERR.NE.0) GO TO 30

C           Initialize system calibration variables. There may be
C           continuity between files, but no real way to tell, so
C           take the safe option.
            CALL SYSINI
         END IF

C        Loop over all scans in current file with pseudo DO-WHILE.
         MORE = .TRUE.
 20      IF (MORE) THEN
            IF (DOLIST) THEN
C              Report summary of current scan
               CALL ATLIST (IFILE+NSKIP, NHDR, FIRST, TOTVIS, TOTFLG,
     *            EOF, IERR)
               IF (IERR.NE.0) GO TO 30
            ELSE IF (SYSDMP) THEN
C              Write SYSCAL information into text files
               CALL TXTWRT (FIRST, TLUN, TFIND, IFILE+NSKIP, NHDR,
     *            EOF, NTXTR, NTXTW, IERR)
               IF (IERR.NE.0) GO TO 30
            ELSE
C              Set up EOP if required
               CALL ATEINI(TLUN, TFIND, IERR)

C              Read scan in
               CALL SCANIN (TTY, DOTCLS, IFILE+NSKIP, NHDR,
     *                      EOF, IERR)
               IF (IERR.NE.0) GO TO 30
            END IF

C           Check TELL Status
            CALL ATTELL (IERR)
            IF (IERR.EQ.2) THEN

C              TELL says finish gracefully
               MORE = .FALSE.
               QUIT = .TRUE.
               IERR = 0
            ELSE IF (IERR.EQ.3) THEN

C              TELL says ABORT
               GOTO 900
            END IF

C           Read the next scan header if we still need more.
            IF (MORE) THEN
               IF (EOF .OR. NHDR.EQ.ESCAN) THEN
                  MORE = .FALSE.
               ELSE
                  CALL READHD (NHDR, IERR)
                  IF (IERR.EQ.-1) THEN
                     IERR = 0
                     EOF = .TRUE.
                     MORE = .FALSE.
                  ELSE IF (IERR.NE.0) THEN
                     GO TO 30
                  END IF
               END IF
            END IF

C           Go and get another scan
            GO TO 20
         END IF

C        This file has been dealt with, begin close down procedures.
         IF (DOLIST) THEN
C           Report grand totals if listing summaries.
            CALL GRAND (IFILE+NSKIP, NHDR-BSCAN+1, FILE, TOTVIS,
     *         TOTFLG)
         END IF

C        Close this RPFITS file.
 30      CALL ATFEOF (NHDR, EOF, JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'The RPFITS file may have been left open.'
            CALL ATMSG (MSGTXT)
            QUIT = .TRUE.
         END IF

C        Close the AIPS files if necessary.
         IF (OPENED) THEN
            IF (OVFLOW.GT.0) THEN
               WRITE (MSGTXT,1030) OVFLOW, OVFLIM
               CALL ATMSG (MSGTXT)
               MSGTXT = '****** LOSS OF ACCURACY MAY HAVE OCCURRED'
               CALL ATMSG (MSGTXT)
            END IF
            CALL ATCLOS (JERR)
            IF (JERR.NE.0) THEN
               MSGTXT ='Some of the AIPS files may have been left open.'
               CALL ATMSG (MSGTXT)
               GO TO 900
            END IF
         END IF
         IF (QUIT) GOTO 900

C        Go and get another file
         IFILE = IFILE + 1
         GOTO 5
      END IF
C
 900  IF (SYSDMP) CALL TXTCLS (NTXTR, NTXTW, TLUN, TFIND)
      IF (DOTCLS) CALL ZCLOSE (TTY(1), TTY(2), IERR)
      IF (NSEVER.GT.0) CALL SEVREP (NSEVER, SEVERE)
      CALL DIE (IERR, BUFF)
      STOP
C-----------------------------------------------------------------------
 1030 FORMAT ('********',I8,' VIS HAVE PEAK > ',I3,' * MEAN **********')
      END



      SUBROUTINE ATPARM (IERR)
C-----------------------------------------------------------------------
C     ATPARM gets the input parameters for task ATLOD, opens and reads
C     the first header record.
C
C     Given:
C          none
C
C     Given via parameters in ATLOD.INC:
C          NCA           I     Number of Compact Array antennas.
C
C     Returned via common ATCTRL:
C          INTAPE        I     Input tape drive number.
C          NSKIP         I     Number of files to skip
C          NFILES        I     Number of files to load.
C          BSCAN         I     First scan to load (same for all files).
C          ESCAN         I     Last scan to load.  Big number if user
C                              doesn't specify
C          NSCANS        I     Number of scans to load (same for all
C                              files).
C          OUTSEQ        I     Output sequence number supplied by user.
C          OUTDSK        I     Output disk number.
C          DOLIST        L     If true, just list a summary of the file
C                              and don't load it.
C          LISLEV        I     List level
C                                 0: file summary
C                                 1: scan summary
C          NSOURC        I     Number of sources in source list.
C          DOSWNT        L     If true, sources are selected, otherwise
C                              rejected.
C          TIMRNG(2)     R     Time range selected, days.
C          NFREQ         I     Number of frequencies selected by user
C          FREQS         D     Selected frequencies
C          BCHAN         I     First channel to load
C          DOUVCM        L     If true, use compressed uv data format.
C          DOSTOK        L     If true convert linear polarizations to
C                              Stokes.
C          DORRLL        L     Call the polarizations RR,LL,RL,LR.
C          DROFLG        L     If true drop flagged visibilities .
C          DOAUTO        L     If true, retain autocorrelations.
C          SHADOW        R     Shadowing diameter, in meters.
C          NXGAP         R     Maximum gap before starting a new scan
C                              index (NX) entry, in days.
C          NXSPAN        R     Maximum length of a scan index (NX)
C                              entry, in days.
C          CLSPAN        R     Interval for CL entries, in days.
C          DOTSYS        I     -1  => Undo on-line correction
C                               0  => Do nothing
C                               N  => Undo on-line Tsys and redo
C                                     with N averaged Tsys measurements
C          NSTAKP        I     Number of integration periods to average
C                              XY phases over for Stokes conversion.
C          DOXYFL        L     If true drop visibilties when XY
C                              phase sbad.
C          EXBSLN(NCA,NCA)
C                        L     Baselines to be dropped.
C          PRTSYS        I        1: List system cal info
C                                 2: Warn about accumulator resets
C                                 3: Warn about bad sampler stats
C                                 4: 1,2 & 3
C          DOSNEG        L     If true, negate system XY cal phases.
C          DONPHA        L     If true, negate visibility phases.
C          USERXY        L     If true, use user given XYPHASE array
C          YGNROT        L     If true, apply XY Phases to Y gain
C
C
C     Returned via common CHRCOM:
C          INFILE        C*48  Disk file containing RPFITS data (if
C                              any).
C          OUTNAM        C*12  Output uv name supplied by user.
C          OUTCLS        C*6   Output uv class supplied by user.
C          SOURCS(30)    C*16  Source selection (or rejection) list.
C
C     Returned via common ATUVIO:
C          NUVOPN        L     Number of times the uv file has been
C                              opened.
C          DOHIST        L     True if history file is open and
C                              ready for text.
C
C     Returned via common VISBUF:
C          BASLEN(,)     R     Compact array baselines, in meters.
C          BASMIN        R     Minimum baseline length, in meters.
C
C     Returned via common ATSYSC:
C          JULBAS        D     Julian date when baseline naming
C                              convention changed from 2-1 to 1-2
C          JULSYS        D     Julian date when Tsys correction can
C                              be made offline.
C          JULSRC        D     SC_SRCNO available for SYSCAL data
C     Returned:
C          IERR          I     Error status, 0 means successfull
C
C     Called:
C          APLGEN: {ZDCHIN}
C          APLSUB: {FILL, GTPARM, H2CHR, HIINIT, IROUND, MSGWRT, RELPOP,
C                   VHDRIN}
C          LOCAPLNOT: {STRIM}
C
C     Algorithm:
C          Just reads and interprets the AIPS input adverbs.
C
C     Notes:
C       1)
C
C
C-----------------------------------------------------------------------
      LOGICAL   GOTONE
      INTEGER   EXBS, IA1, IA2, IERR, IOBLK(256), IRET, IROUND, J, K, N,
     *          NPARMS, JERR
      CHARACTER PRGM*6, OPTYPE*4

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'

      INTEGER ITEMP, I

      DATA PRGM /'ATLOD '/
C-----------------------------------------------------------------------
C  Do basic AIPS initiation procedures.
C     Initialize I/O.
      CALL ZDCHIN (.TRUE.)
      CALL HIINIT (1)
      CALL VHDRIN
      IERR = 0

C     History file doesn't exist yet.
      NUVOPN = 0
      DOHIST = .FALSE.

C     Initialize the CFILES common.
      NSCR   = 0
      NCFILE = 0

C     Get input ADVERBS.
      NPARMS = 297
      CALL GTPARM (PRGM, NPARMS, RQUICK, XITAPE, IOBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 10) IERR
   10    FORMAT ('ATPARM: ERROR',I3,' OBTAINING INPUT PARAMETERS.')
         CALL ATMSG (MSGTXT)
         IRET = 8
         GO TO 999
      END IF

C  Interpret ADVERB values.
C     Input selection.
      INTAPE = IROUND (XITAPE)
      NSKIP  = IROUND (XNSKIP)
      IF (NSKIP.LT.0) NSKIP = 0
      NFILES = 1

      BSCAN = IROUND (XBSCAN)
      NSCANS = IROUND (XNSCAN)
      IF (BSCAN.LE.0) BSCAN = 1
      IF (NSCANS.LE.0) NSCANS = 10000
      ESCAN = BSCAN + NSCANS - 1

      CALL H2CHR (48, 1, XINFIL, INFILE)
C     Convert filename to lower case?
      IF ( IROUND(XCPARM(8)).GT.0) THEN
         DO 15 J = INDEX(INFILE, ':') + 1, 48
            IF (ICHAR(INFILE(J:J)).GE.ICHAR('A').and.
     +          ICHAR(INFILE(J:J)).LE.ICHAR('Z'))
     +          INFILE(J:J) = CHAR(ICHAR(INFILE(J:J))
     +          - ICHAR('A') + ICHAR('a'))
   15    CONTINUE
         END IF
      AIFILE = INFILE
      CALL H2CHR (12, 1, XOUTNM, OUTNAM)
      CALL H2CHR (6, 1, XOUTCL, OUTCLS)
      OUTSEQ = IROUND (XOUTSQ)
      OUTDSK = IROUND (XOUTDI)

      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF (OPTYPE.EQ.' ') OPTYPE = 'LOAD'

C  Data selection adverbs.
      NSOURC = 0
      DOSWNT = .TRUE.
      DO 30 J = 1, 30
         CALL H2CHR (16, 1, XSOURC(1,J), SOURCS(J))
         IF (SOURCS(J).NE.' ') THEN
            CALL STRIM (SOURCS(J), N)
            IF (SOURCS(J)(1:1).EQ.'-') THEN
               DOSWNT = .FALSE.
               SOURCS(J) = SOURCS(J)(2:)
            END IF

C           Make sure its not already in the list.
            DO 20 K = 1, NSOURC
               IF (SOURCS(J).EQ.SOURCS(K)) GO TO 30
 20         CONTINUE
            NSOURC = NSOURC + 1
            IF (J.GT.NSOURC) SOURCS(NSOURC) = SOURCS(J)
         END IF

C        Just for safety's sake.
         IF (NSOURC.EQ.0) DOSWNT = .FALSE.
 30   CONTINUE

C     Time range selection, convert to days.
      TIMRNG(1) = ((XTIMER(4)/60.0 + XTIMER(3))/60.0 + XTIMER(2))/24.0
      TIMRNG(1) = TIMRNG(1) + XTIMER(1)
      TIMRNG(2) = ((XTIMER(8)/60.0 + XTIMER(7))/60.0 + XTIMER(6))/24.0
      TIMRNG(2) = TIMRNG(2) + XTIMER(5)
      IF (TIMRNG(1).EQ.0.0) TIMRNG(1) = -9999.0
      IF (TIMRNG(2).EQ.0.0) TIMRNG(2) = +9999.0
      IF (TIMRNG(2).LE.TIMRNG(1)) THEN
         MSGTXT = 'ATPARM: INVALID TIME RANGE, ABORT!'
         CALL ATMSG (MSGTXT)
         IRET = 8
         GO TO 999
      END IF

C     Map RPFITS IF chains to IF axis ?
      IFMAP = XIFMAP.GT.0.0

C     Frequency selection, convert MHz to Hz.
      NUMIF = MIN(MAX_SIM, IROUND(XNIF))
      NFREQS = 0
      DO 40 I = 1, 30
         IF (XSELFR(I).GT.0.0) THEN
            NFREQS = NFREQS + 1
            FREQS(NFREQS) = XSELFR(I) * 1.0D6
         ELSE
            GOTO 41
         END IF
 40   CONTINUE


C     IF selection
 41   NIFS = 0
      DO 42 I = 1, 5
         IF (XSELIFN(I).GT.0.0) THEN
            NIFS = NIFS + 1
            IFS(NIFS) = IROUND(XSELIFN(I))
         ELSE
            GOTO 45
         END IF
 42   CONTINUE

C  Control adverbs.
C     List a summary of data base without loading it?
 45   DOLIST = .FALSE.
      IF (OPTYPE.EQ.'SUMM') THEN
         DOLIST = .TRUE.
         LISLEV = 0
      ELSE IF (OPTYPE.EQ.'LIST') THEN
         DOLIST = .TRUE.
         LISLEV = 1
      ELSE IF (OPTYPE.EQ.'SYSC') THEN
         SYSDMP = .TRUE.
      END IF
      IF (SYSDMP .OR. DOLIST) GO TO 160

C     Use compressed uv data format?
      DOUVCM = IROUND(XDOUVC).GT.0
      IF (DOUVCM) THEN
         MSGTXT = 'Using compressed uv data format.'
         CALL ATMSG (MSGTXT)
      END IF

C     Convert linear polarizations to Stokes parameters?
      DOSTOK = IROUND(XAPARM(1)).GT.0

C     Call Linear polarizations RR,LL,RL,LR?
      DORRLL = IROUND(XAPARM(1)).LT.0

C     Drop flagged data?
      DROFLG = XAPARM(2).LE.0.0
      IF (DROFLG) THEN
         MSGTXT = 'Dropping all flagged visibilities.'
      ELSE
         MSGTXT = 'Retaining all flagged visibilities.'
      END IF
      CALL ATMSG (MSGTXT)

C     Retain autocorrelations?
      DOAUTO = IROUND(XAPARM(3)).GT.0
      IF (.NOT.DOAUTO) THEN
         MSGTXT = 'Dropping autocorrelations.'
      ELSE
         MSGTXT = 'Retaining autocorrelations.'
      END IF
      CALL ATMSG (MSGTXT)

C     Shadowing diameter.
      SHADOW = MIN(XAPARM(4), 88.0)
      IF (SHADOW.EQ.0.0) SHADOW = 22.0

C     Index and calibration table control.
      NXGAP  = XAPARM(5)
      IF (NXGAP.LT.1.0) NXGAP = 10.0
      NXGAP  = NXGAP/(24.0*60.0)

      NXSPAN = XAPARM(6)
      IF (NXSPAN.LT.1.0) NXSPAN = 60.0
      NXSPAN = NXSPAN/(24.0*60.0)

      CLSPAN = XAPARM(7)
      IF (CLSPAN.LT.(2.0/60.0)) CLSPAN = 1.0
      CLSPAN = CLSPAN/(24.0*60.0)

C     Make Tsys correction ?
      DOTSYS = IROUND(XAPARM(8))
      IF (DOTSYS.EQ.-1) THEN
         MSGTXT = 'Undoing on-line Tsys correction'
         CALL ATMSG (MSGTXT)
      ELSE IF (DOTSYS.GT.0) THEN
         MSGTXT = 'Applying off-line Tsys correction.'
         CALL ATMSG (MSGTXT)

         IF (DOTSYS.GT.MAXTST) THEN
            DOTSYS = MAXTST
            WRITE (MSGTXT, 50) MAXTST
 50         FORMAT ('WARNING! Reducing Tsys average time to ',I3,
     *              ' integrations')
            CALL ATMSG (MSGTXT)
         END IF
      ELSE
         DOTSYS = 0
      END IF


C  Any baselines to be excluded?
      DO 70 J = 1, NCA
         DO 60 K = 1, NCA
            EXBSLN(J,K) = .FALSE.
 60      CONTINUE
         IF (.NOT.DOAUTO) EXBSLN(J,J) = .TRUE.
 70   CONTINUE

      GOTONE = .FALSE.
      DO 90 J = 1, 10
         EXBS = IROUND(XBPARM(J))
         IF (EXBS.EQ.0) GO TO 90

         IA1 = ABS(EXBS)/10
         IA2 = ABS(EXBS) - 10*IA1

         IF (IA1.GT.NCA) GO TO 90
         IF (IA2.GT.NCA) GO TO 90

         IF (IA1.EQ.0 .OR. IA2.EQ.0) THEN
            DO 80 K = 1, NCA
               EXBSLN(K,IA2) = .TRUE.
               EXBSLN(IA2,K) = .TRUE.
 80         CONTINUE
         ELSE IF (EXBS.LT.0) THEN
            EXBSLN(IA1,IA2) = .TRUE.
         ELSE IF (EXBS.GT.0) THEN
            EXBSLN(IA1,IA2) = .TRUE.
            EXBSLN(IA2,IA1) = .TRUE.
         END IF

         GOTONE = .TRUE.
 90   CONTINUE

      IF (GOTONE) THEN
C        Tell the world.
         MSGTXT = 'Baselines (I,J) to be dropped by request:'
         CALL ATMSG (MSGTXT)

         WRITE (MSGTXT, 100) (J, J=1,NCA)
  100    FORMAT (10X,'I/J',9(I2,:))
         CALL ATMSG (MSGTXT)

         DO 130 IA1 = 1, NCA
            WRITE (MSGTXT, 110) IA1, (EXBSLN(IA1,IA2), IA2=1,NCA)
  110       FORMAT (10X,I2,1X,9(L2,:))
            DO 120 J = 14, 80
               IF (MSGTXT(J:J).EQ.'T') THEN
                  MSGTXT(J:J) = '*'
               ELSE IF (MSGTXT(J:J).EQ.'F') THEN
                  MSGTXT(J:J) = '-'
               END IF
  120       CONTINUE
            CALL ATMSG (MSGTXT)
  130    CONTINUE
      END IF

C     Initialize baseline array.
      DO 150 IA2 = 1, NCA
         DO 140 IA1 = 1, NCA
            BASLEN(IA1,IA2) = 1E20
  140    CONTINUE
  150 CONTINUE
      BASMIN = 1E20


C     List system calibration information
      PRTSYS = IROUND(XCPARM(1))

C     Negate calibration XY phase diff (the empirical approach
C     to Stokes parameters)?
      DOSNEG = IROUND(XCPARM(2)).GT.0
      IF (DOSNEG) THEN
         MSGTXT = 'Negating system XY phase difference.'
         CALL ATMSG (MSGTXT)
      END IF

C     Negate visibility phases?
      DONPHA = IROUND(XCPARM(3)).GT.0
      IF (DONPHA) THEN
         MSGTXT = 'The visibilty phases will be negated.'
         CALL ATMSG (MSGTXT)
      END IF

C     User given XYPHASE array used rather than on-line XY phases
      USERXY = IROUND(XCPARM(4)).GT.0

C     Apply XY phase to Y gain without Stokes conversion ?
      YGNROT = IROUND(XCPARM(5)).GT.0
      IF (YGNROT .AND. DOSTOK) THEN
         MSGTXT = 'Requests for Stokes conversion and Y gain'//
     *            ' rotation incompatible'
         CALL ATMSG (MSGTXT)
         IERR = 1
         GOTO 999
      END IF

C     Check for bad sampler statistics
      SSCHK = IROUND(XCPARM(6))
      IF (SSCHK.LT.0 .OR. SSCHK.GT.4) SSCHK = 0

C     Convert user given XY phases to degrees
      IF (USERXY) THEN
         MSGTXT = 'User supplied XY phases used in place of '//
     *            'on-line values'
         CALL ATMSG (MSGTXT)

C        Convert user given XY phases to radians
         DO 155 J = 1, 6
            DO 154 K = 1, 6
               XYPHAS(K,J) = XYPHAS(K,J) / R2D
  154       CONTINUE
  155    CONTINUE
      END IF

C     Size of XY phase difference stack.
      ITEMP = IROUND(XAPARM(9))
      IF (ITEMP.LE.1) THEN
         NSTAKP = 1
      ELSE
         IF (ITEMP.GT.MAXPST) THEN
            NSTAKP = MAXPST
            WRITE (MSGTXT, 158) MAXPST
  158       FORMAT ('Reducing XY phase average time to ',
     *              I3,' integrations')
            CALL ATMSG (MSGTXT)
         ELSE
            NSTAKP = ITEMP
         END IF
      END IF

C     Edit data based upon XY phase difference drop outs ?
C     May use internal clipping tolerances or aparm(10) if < 0.
C     We allow XY phase editing and user input XYPHASE array
      DOXYFL = .FALSE.
      PHSCLP = 0.0
      IF (XAPARM(10).NE.0.0) THEN
         IF (XAPARM(10).LT.0.0) PHSCLP = ABS(XAPARM(10)) / R2D
         DOXYFL = .TRUE.
         MSGTXT = 'Dropping visibilities when XY phase '//
     *            'differences bad'
         CALL ATMSG (MSGTXT)
         IF (USERXY) THEN
            IF (XAPARM(10).GT.0.0) THEN
               MSGTXT = 'Because the XYPHASE array is being used'
               CALL ATMSG (MSGTXT)
               MSGTXT = 'as the mean XYPHASE value, you must set'
               CALL ATMSG (MSGTXT)
               MSGTXT = 'a tolerance with APARM(10) < 0'
               CALL ATMSG (MSGTXT)
               IRET = 8
               GOTO 999
            ELSE
               MSGTXT = 'XYPHASE array used as mean value for'
               CALL ATMSG (MSGTXT)
               MSGTXT = 'XY phase clipping test'
               CALL ATMSG (MSGTXT)
            END IF
         END IF
      END IF

C     Fudges
      TSYFDG = IROUND(XCPARM(10)).GT.0 .AND. XAPARM(8).GT.0
      IF (TSYFDG) THEN
         MSGTXT = '****************************************'
         CALL ATMSG (MSGTXT)
         MSGTXT = 'WARNING: APPLYING Tsys from IF 1 to IF 2'
         CALL ATMSG (MSGTXT)
         MSGTXT = '****************************************'
         CALL ATMSG (MSGTXT)
      END IF

      IF (XDPARM(1).GT.0.0) THEN
         MSGTXT = '**********************************************'
         CALL ATMSG (MSGTXT)
         MSGTXT = 'WARNING: Setting all scan based source IDs = 1'
         CALL ATMSG (MSGTXT)
         MSGTXT = '**********************************************'
         CALL ATMSG (MSGTXT)
      END IF

C     Hanning smooth
      HANN = .FALSE.
      IF (XDPARM(2).GT.0.0 .AND. OPTYPE.EQ.'LOAD') THEN
         HANN = .TRUE.
         MSGTXT = 'Spectra will be 3-point Hanning smoothed'
         CALL ATMSG (MSGTXT)
      END IF

C     Weights
      IF (OPTYPE.EQ.'LOAD') THEN
         IF (XDPARM(3).GT.0.0) THEN
            WRITE (MSGTXT, 159) XDPARM(3)
  159       FORMAT ('Visibility weights set to ', F6.3)
         ELSE
            MSGTXT = 'Vis weights set to integration time '//
     *               'in units of 15 sec'
         END IF
         CALL ATMSG (MSGTXT)
      END IF

      FLGFDG = IROUND(XDPARM(4)).GT.0
      IF (FLGFDG) THEN
         MSGTXT = '****************************************'
         CALL ATMSG (MSGTXT)
         MSGTXT = 'WARNING: APPLYING flagging from IF 1 to IF 2'
         CALL ATMSG (MSGTXT)
         MSGTXT = '****************************************'
         CALL ATMSG (MSGTXT)
      END IF

C     Pulsar binning
      PBIN1 = 0
      PBIN2 = 0
      PBIN3 = 1
Crgd >
      dm=0.0
      period=0.0
      nbins=0
Crgd <
      IF (IROUND(XDPARM(5)).GT.0) THEN
         PBIN1 = IROUND(XDPARM(5))
         IF (IROUND(XDPARM(6)).GT.0) PBIN2 = IROUND(XDPARM(6))
         IF (IROUND(XDPARM(7)).GT.0) PBIN3 = IROUND(XDPARM(7))
Crgd >
         IF (XDPARM(9).NE.0.0) dm = XDPARM(9)
         IF (IROUND(XDPARM(10)).GT.0) nbins = INT(XDPARM(10))
Crgd Alternativily ....
Crgd         IF (IROUND(XDPARM(10)).GT.0) nbins =
Crgd     :      2**int(log(XDPARM(10))/log(2))
         IF (XDPARM(10).GT.0.0) period = XDPARM(10)-nbins
C   So this only works where period < 1
Crgd improves neatness in loops
         if (PBIN2.EQ.0) PBIN2=PBIN1
Crgd <
         WRITE (MSGTXT, 161) PBIN1, PBIN2, PBIN3
161      FORMAT ('Averaging pulsar bins',I4,' to',I4,' in steps of',I4)
         CALL ATMSG (MSGTXT)
      ELSE
         WRITE (MSGTXT, 162)
162      FORMAT ('Averaging all pulsar bins')
         CALL ATMSG (MSGTXT)
      END IF

      RCMPUV = IROUND(XDPARM(8))

      IF (RCMPUV.GT.0) THEN
         WRITE (MSGTXT, 165)
 165     FORMAT ('Recomputing all UVWs')
         CALL ATMSG (MSGTXT)
      ELSE IF (RCMPUV.EQ.0) THEN
C         WRITE (MSGTXT, 166)
C 166     FORMAT ('Recomputing UVWs for pre-20/02/1998 LBA data only')
C         CALL ATMSG (MSGTXT)
      END IF

C     Initialize catalog header.
      CALL FILL (256, 0, CATBLK)
      CATBLK(KIIMU) = NLUSER

C     Test legality of tape input.
  160 IRET = 0
      IF (INFILE.EQ.' ') THEN
C        Input comes from tape.
         NFILES = IROUND (XNFILE)
         IF (NFILES.LE.0) NFILES = 1
         IF (INTAPE.EQ.0) INTAPE = 1
         IF (INTAPE.LT.1 .OR. NTAPED.LT.INTAPE) THEN
            WRITE (MSGTXT, 170) INTAPE
  170       FORMAT ('ATPARM: INTAPE =',I7,' IS NOT RECOGNIZED.')
            CALL ATMSG (MSGTXT)
            IRET = 8
         END IF

         IF (NINTRN.LT.NPOPS) THEN
            MSGTXT = 'ATPARM: TAPE JOBS ARE NOT ALLOWED IN BATCH.'
            CALL ATMSG (MSGTXT)
            IRET = 8
         END IF
      END IF


C     Initialize dates when AT conventions changed
C     Baselines renamed
      CALL JULDAY (DATBAS, JULBAS)

C     Off line Tsys correction possible
      CALL JULDAY (DATTSY, JULSYS)

C     Sampler statistics all 17.1% after this date
      CALL JULDAY (DATSS1, JULSS1)

C     Sampler statistics all 17.3% after this date
      CALL JULDAY (DATSS2, JULSS2)

C     Sampler corrections done in correlator after this date
      CALL JULDAY (DATSSC, JULSSC)

C     SC_SRCNO available for SYSCAL data
      CALL JULDAY (DATSRC, JULSRC)

C     Valid UVW values available for LBA data
      CALL JULDAY (DATUVW, JULUVW)

C     Restart AIPS.
 999  IF (RQUICK .AND.(PRTSYS.NE.1 .AND. PRTSYS.NE.3))
     *    CALL RELPOP (IRET, IOBLK, JERR)
      IF (IERR.EQ.0) IERR = JERR


      RETURN
      END



      SUBROUTINE BLDNAM (IERR)
C-----------------------------------------------------------------------
C     Build the file name, dealing correctly with tape and disk files.
C
C     Given via common ATCTRL:
C          INTAPE        I     AIPS Tape drive number
C
C     Given via common CHRCOM:
C          INFILE        C     User specified file name
C
C     Given and returned via RPFITS common NAMES:
C          FILE          C     Full input file specification to be
C                              passed to RPFITSIN.
C     Returned:
C          IERR          I     Error status, 0 means success.
C
C     Called:
C          APLGEN: {ZFULLN, ZTRLOG}
C          APLSUB: {MSGWRT}
C
C
C-----------------------------------------------------------------------

      INTEGER   IERR, LENF, LENX
      CHARACTER TAPE*6

      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
      IF (INFILE.EQ.' ') THEN
         WRITE (TAPE, 10) INTAPE
   10    FORMAT ('TAPE', I1.1)
C 10      FORMAT ('MT', I2.2)
         LENF = LEN(FILE)
         tape(6:6)=' '
         write (6,*) 'ATLOD: Using TAPE ',tape(1:5)
         CALL ZTRLOG (6, TAPE, LENF, FILE, LENX, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 20) IERR
 20         FORMAT ('BLDNAM: ERROR',I3,' TRANSLATING TAPE DRIVE NAME.')
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF
      ELSE
         CALL ZFULLN (INFILE, 'FITS', ' ', FILE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 30) IERR
 30         FORMAT ('BLDNAM: ERROR',I3,' TRANSLATING INPUT FILE NAME.')
            CALL ATMSG (MSGTXT)
         END IF
      END IF

 999  RETURN
      END




      SUBROUTINE SKIPF (INTAPE, NSKIP, IERR)
C-----------------------------------------------------------------------
C     Skip files on tape.
C
C     Given:
C          INTAPE        I     Tape number
C          NSKIP         I     Number of files to skip for current
C                              location on tape.
C     Returned:
C          IERR          I     Error status, 0 means success.
C
C     Called:
C          APLSUB: {MSGWRT}
C          APLGEN: {ZTAPE}
C
C-----------------------------------------------------------------------
      INTEGER I, IERR, TLUN, TIND, NSKIP, INTAPE
      CHARACTER TNAME*48

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C     Open tape
      TLUN = 129 - INTAPE
      CALL ZPHFIL ('MT', INTAPE, 1, 1, TNAME, IERR)
      CALL ZTPOPN (TLUN, TIND, INTAPE, TNAME, 'READ', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,100) IERR
  100    FORMAT ('SKIPF: Error ', I2, ' opening tape')
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF

C     Advance tape files
      IF (NSKIP.GT.0) THEN
         DO 300 I = 1, NSKIP
C
            WRITE (MSGTXT, 150) I
  150       FORMAT ('SKIPF: Skipping file #', I3)
            CALL ATMSG (MSGTXT)

C           Ansi files have header, data and trailer
            CALL ZTAPE ('ADVF', TLUN, TIND, 3, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 200) IERR
 200           FORMAT ('SKIPF: Error ', I2, ' skipping tape files')
               CALL ATMSG (MSGTXT)
               GOTO 999
            END IF

C           Check TELL Status; user may change mind about number
C           of files to skip
            CALL ATTELL (IERR)
            IF (IERR.GT.1) GOTO 999
            IERR = 0
 300     CONTINUE
C
         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         MSGTXT = 'SKIPF: Finished skipping files.'
         CALL ATMSG (MSGTXT)
      END IF

C     Close tape
      CALL ZTPCLS (TLUN, TIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 400) IERR
 400     FORMAT ('SKIPF: Error ', I2, ' closing tape')
         CALL ATMSG (MSGTXT)
      END IF
C
 999  RETURN
      END


      SUBROUTINE SKIPS (BSCAN, NHDR, IERR)
C-----------------------------------------------------------------------
C     SKIPS skips scans in an RPFITS file.
C
C     Given:
C          BSCAN         I     First scan number to read
C
C     Returned:
C          NHDR          I     Number of headers read so far.
C                              Initialized to 0 here if no skipping
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          ATLOD:  {READHD}
C          APLSUB: {MSGWRT}
C
C     Notes:
C       1) On exit from this routine, the DATA for the last scan skipped
C          has NOT yet been skipped, only its HEADER has been read.  The
C          next call should therefore be to READHD, which will find the
C          header of the next scan, skipping the desired data.
C
C
C-----------------------------------------------------------------------

      INTEGER BSCAN, I, IERR, NHDR

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NHDR = 0
      IF (BSCAN.GT.1) THEN
         DO 20 I = 1, BSCAN-1
            CALL READHD (NHDR, IERR)
            IF (IERR.EQ.0) THEN
               WRITE (MSGTXT, 10) I
   10          FORMAT ('Skipping scan number',I4)
               CALL ATMSG (MSGTXT)
            ELSE
               GO TO 999
            END IF
   20    CONTINUE
      END IF

  999 RETURN
      END


      SUBROUTINE HEDHNT (IFILE, NHDR, EOF, IERR)
C-----------------------------------------------------------------------
C     Find the first header which contains IF and SU tables and
C     contains the frequencies and sources asked for by the user
C
C  Input:
C    IFILE     I    Number of current file
C  Input/output:
C    NHDR      I    Number of headers read so far
C
C  Output:
C    EOF       L    True if EOF encountered
C    IERR      I    0  => OK
C                   -1 => Ran out of scans as specified by ESCANS
C                         before finding match
C                   Otherwise error
C
C-----------------------------------------------------------------------
      INTEGER NHDR, IERR, IFILE
      LOGICAL EOF
CC
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      INTEGER NTAB
      LOGICAL MORE
C-----------------------------------------------------------------------
      IERR = -1
      NTAB = 0
      MORE = NHDR.LT.ESCAN
C
  100 IF (MORE) THEN
         IERR = 0
         CALL READHD (NHDR, IERR)
         IF (IERR.EQ.-1) THEN
C
C           Zero length file encountered ?
            EOF = .TRUE.
            IF (NTAB.EQ.0) THEN
               WRITE (MSGTXT, 200) IFILE
  200          FORMAT ('File ',I4,' is of zero length.')
               CALL ATMSG (MSGTXT)
            END IF
            IERR = 0
            MORE = .FALSE.
         ELSE IF (IERR.NE.0) THEN
            MORE = .FALSE.
         ELSE
            NTAB = NTAB + 1
            IF (.NOT.IF_FOUND .OR. .NOT.SU_FOUND) THEN
               WRITE (MSGTXT, 300) NHDR
 300           FORMAT ('HEDHNT: Scan ', I3,
     *                 ' has no IF and/or SU tables; skip it')
               CALL ATMSG (MSGTXT)
            ELSE
C
C              This header has IF and SU tables.  See if it contains
C              any frequencies or sources of interest
               CALL IFMACH (IERR)
               IF (IERR.NE.0) RETURN
C
               CALL SUMACH (IERR)
               IF (IERR.NE.0) RETURN
               IF (NIFSEL*NSUSEL.GT.0) THEN
                  MORE = .FALSE.
               ELSE
                  WRITE (MSGTXT, 400) NHDR
 400              FORMAT ('HEDHNT: No frequencies or sources selected',
     *                    ' in scan ', I3)
                  CALL ATMSG (MSGTXT)
               END IF
            END IF
         END IF

C        No more scans requested by user
         IF (NHDR.GE.ESCAN) THEN
            MORE = .FALSE.
            IF (NIFSEL*NSUSEL.EQ.0) IERR = -1
         END IF
         GOTO 100
      END IF
C
      RETURN
      END




      SUBROUTINE IFMACH (IERR)
C-----------------------------------------------------------------------
C     Try and match the current RPFITS IF table with user's selected
C     frequencies and IFs, if any.
C
C
C   Inputs via ATLOD parameters:
C    FRQTOL    D     Tolerance to match frequency with
C
C   Input via common ATCTRL:
C    NFREQS    I     NUmber of frequencies to select, as specifed
C                    by the user.  0 means want all frequencies.
C    FREQS     D     Frequencies to match (Hz)
C    NIFS      I     Number of IFs to select as given by the user.
C                    0 means all IFs
C    IFS       I     IFs to select given by the user.
C    IFMAP     L     If true, then map the RPFITS CHAIN number to
C                    the AIPS IF Axis location directly.  Otherwise
C                    ATLOD tries to guess at where to put it
C
C   Input via RPFITS parameters:
C    MAX_IF    I     Maximum number of frequencies allowed in
C                    RPFITS scan.
C    MAX_SIM   I     Maximum number of simultaneous frequencies allowed
C
C   Input via RPFITS common IF:
C    NNIF      I     Number of entries in RPFITS IF table
C    IF_FREQ   D     RPFITS IF table
C    IFNUMS    I     RPFITS IF number table
C    IF_SIMUL  I     RPFITS IF simultaneity number table (a local
C                    FREQID, if you like)
C    IF_CHAIN  I     Number of frequency chain for each frequency.
C
C   Output via common ATSEL:
C    NIFSEL    I     Number of groups of simultaneous frequencies
C                    found in users list of frequencies
C    SELIF     I     List of pointers to frequencies in RPFITS IF
C                    table that user wants for each simultaneous group
C                    of frequencies.  Thus, SELIF(2,3) would point
C                    at an entry in the IF_* tables for the third
C                    frequency of the second simultaneous group
C                    of frequencies in the scan.
C    SELNIF    I     Number of frequencies in each simultaneous group
C    SELAX     I     Conversion from IFNUM to AIPS IF axis value
C                    for the selected frequencies
C
C
C   Output
C    IERR      I     0 -> OK, 1 -> IFNUMS madness
C
C    Notes:
C      Assumes RPFITS IF table is structured with each group of
C      simultaneous frequencies listed sequentially
C
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER IERR
CC
      INTEGER IFRQ, JFRQ, IFSIM, LIFSIM
      LOGICAL WANT
C-----------------------------------------------------------------------
C     Initialize
      NIFSEL = 0
      DO 30 JFRQ = 1, MAX_SIM
         DO 25 IFRQ = 1, MAX_IF
            SELIF(IFRQ,JFRQ) = -1
 25      CONTINUE
 30   CONTINUE
C
      DO 40 IFRQ = 1, MAX_IF
         SELAX(IFRQ)  = -1
         SELNIF(IFRQ) =  0
 40   CONTINUE


C     Loop over all entries in RPFITS IF table and assemble
C     list of simultaneous frequencies

      IERR = 0
      LIFSIM = 0
      DO 50 IFRQ = 1, NNIF

C        IFNUMS ???  What is it for ?? Is it an extra layer of
C        misdirection ?? In practice, we always write
C        IFNUMS(I) = I so let's ignore IFNUMS
         IF (IFNUMS(IFRQ).NE.IFRQ) THEN
            MSGTXT = 'IFMACH: I do not understand the RPFITS IF TABLE'
            CALL ATMSG (MSGTXT)
            MSGTXT = 'IFMACH: Go and see NEBK and yell at RPN'
            CALL ATMSG (MSGTXT)
            IERR = 1
            RETURN
         END IF

C        Find which simultaneous IF chain this frequency is from
         IFSIM = IF_SIMUL(IFRQ)

C        Do we want this frequency and IF ?
         CALL FQMACH (FRQTOL, IF_FREQ(IFRQ), IF_CHAIN(IFRQ), NFREQS,
     *                FREQS, NIFS, IFS, WANT)

         IF (WANT) THEN

C           If this is a new group/individual, increment the number
C           of groups of simultaneous frequencies
            IF (IFSIM.NE.LIFSIM) NIFSEL = NIFSEL + 1

C           Specify how many simultaneous frequencies there are
C           for this group so far
            SELNIF(NIFSEL) = SELNIF(NIFSEL) + 1

C           Specify which value on the AIPS IF axis this
C           frequency IFNUM is going to.
            IF (IFMAP) THEN

C              Map IF chain number to IF axis
               SELAX(IFRQ) = IF_CHAIN(IFRQ)
            ELSE

C              Use next available IF axis location
               SELAX(IFRQ) = SELNIF(NIFSEL)
            END IF

C           Specify which frequency in the IF table each member
C           of each group of frequencies converts to
            SELIF(NIFSEL,SELNIF(NIFSEL)) = IFRQ

C           Update last pair indicator
            LIFSIM = IFSIM
         END IF

 50   CONTINUE

C
      RETURN
      END



      SUBROUTINE FQMACH (FRQTOL, FREQ, IF, NFREQS, FREQS,
     *                   NIFS, IFS, WANT)
C--------------------------------------------------------------------
C     Match a frequency and IF with a list of frequencies and IFs
C
C  Input:
C    FRQTOL  D    Tolerance for frequency match
C    FREQ    D    Freq. of interest
C    IF      I    IF of interest
C    NFREQS  I    Number of freqs. in list.  If zero, we want all.
C    FREQS   D    List of freqs
C    NIFS    I    Number of IFs in list.  If zero, we want them all.
C    IFS     I    List of IFs
C  Output:
C    WANT    L    True if this frequency and IF wanted
C
C-------------------------------------------------------------------
      INTEGER NFREQS, NIFS, IFS(*), IF
      DOUBLE PRECISION FREQ, FREQS(*), FRQTOL
      LOGICAL WANT
CC
      INTEGER I
C-------------------------------------------------------------------
C     See if this frequency is in the frequency list
      WANT = .FALSE.
      IF (NFREQS.EQ.0) THEN
         WANT = .TRUE.
      ELSE
         DO 10 I = 1, NFREQS
            IF (ABS(FREQS(I)-FREQ).LE.FRQTOL) THEN
               WANT = .TRUE.
               GOTO 15
            END IF
 10      CONTINUE
      END IF

C     If this frequency is wanted, see if it is in the IF list
 15   IF (WANT) THEN
         IF (NIFS.GT.0) THEN
            DO 20 I = 1, NIFS
               IF (IF.EQ.IFS(I)) GOTO 999
 20         CONTINUE
            WANT = .FALSE.
         END IF
      END IF
C
 999  RETURN
      END



      SUBROUTINE SUMACH (IERR)
C-----------------------------------------------------------------------
C     Match sources in RPFITS SU table with user's given list
C
C   Input via common ATCTRL:
C     DOSWNT    L    True if user specified sources are wanted
C     NSOURC    I    Number of user specified sources
C
C   Input via common CHRCOM
C     SOURCS    C    List of user specified sources
C
C   Input via RPFITS common SU:
C     NNSU      I    Number of sources in RPFITS SU table
C     SU_NAME   C    Source names in RPFITS SU table
C
C  Output via common ATSEL:
C     NSUSEL    I    Number of sources matched in RPFITS SU table
C     SELSRC    I    List of pointers to sources in RPFITS SU table
C                    that user WANTS (thus DOSWNT has been
C                    dealt with).
C  OUtput
C     IERR      I    0 -> OK, 1 -> SUNUM madness
C
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER IERR
CC
      INTEGER ISRC, JSRC
C-----------------------------------------------------------------------
C     SUNUM ???  What is it for ?? Is it an extra layer of
C     misdirection ?? In practice, we always write
C     SUNUM(I) = I so let's ignore SUNUM
      DO 10 ISRC = 1, NNSU
         IF (SUNUM(ISRC).NE.ISRC) THEN
            MSGTXT = 'SUMACH: I do not understand the RPFITS SU TABLE'
            CALL ATMSG (MSGTXT)
            MSGTXT = 'SUMACH: Go and see NEBK and yell at RPN'
            CALL ATMSG (MSGTXT)
            IERR = 1
            RETURN
         END IF
 10   CONTINUE
C
      NSUSEL = 0
      IF (DOSWNT) THEN

C        Source list contains selected sources.
         DO 50 ISRC = 1, NNSU

C           Match this source in RPFITS table with user list
            DO 40 JSRC = 1, NSOURC
               IF (SU_NAME(ISRC).EQ.SOURCS(JSRC)) THEN
                  NSUSEL = NSUSEL + 1
                  SELSRC(NSUSEL) = ISRC
               END IF
 40         CONTINUE
 50      CONTINUE

      ELSE

C        Source list contains rejected sources.
         DO 100 ISRC = 1, NNSU
            IF (NSOURC.EQ.0) THEN
               NSUSEL = NSUSEL + 1
               SELSRC(NSUSEL) = ISRC
            ELSE
               DO 90 JSRC = 1, NSOURC
                  IF (SU_NAME(ISRC).EQ.SOURCS(JSRC)) GOTO 100
 90            CONTINUE
               NSUSEL = NSUSEL + 1
               SELSRC(NSUSEL) = ISRC
            END IF
 100     CONTINUE

      END IF
C
      RETURN
      END


      SUBROUTINE SETUV (IERR)
C-----------------------------------------------------------------------
C     SETUV builds a catalog header from the RPFITS header records, and
C     creates or reopens the output uv file.  Creates the history file
C     and calls ATHI to record basic information.
C
C     Given via common ATCTRL:
C          OUTSEQ        I     Output sequence number supplied by user.
C          OUTDSK        I     Output disk number.
C
C     Given via common CHRCOM:
C          OUTNAM        C*12  Output uv name supplied by user.
C          OUTCLS        C*6   Output uv class supplied by user.
C
C
C     Given via RPFITS common ANTEN:
C          NANT          I     The number of antennas.
C
C     Returned via common ATCTRL:
C          FILSEQ        I     Actual output uv sequence number.
C          SELIF         I     Pointers to matched frequencies in
C                              RPFITS IF table for current header
C          SELSRC        I     Pointers to matched sources in
C                              RPFITS SU table for current header.
C
C     Returned via common CHRCOM:
C          FILNAM        C*12  Actual output uv name.
C          FILCLS        C*6   Actual output uv class.
C
C     Returned via common ATUVIO and ATUVCH:
C          CNO           I     UV file catalogue slot number.
C          NBLKS         I     Number of blocks in UV file.
C          UVNAME        C*48  AIPS UV file name.
C          UVLUN         I     UV file logical unit number.
C          UVFIND        I     FTAB UV file pointer returned by ZOPEN.
C          LREC          I     Number of values in a visibility record.
C          NPIO          I     Maximum uv records per transfer.
C          UVBIND        I     Pointer into UVBUFF.
C          NIO           I     Number of visibilities to be written.
C          VISCNT        I     Total number of visibilities in the UV
C          TBSORT        L     True if the data is in time sequence.
C          REFREQ        D     Reference frequency, (Hz).
C          HILUN         I     HI file logical unit number.
C
C     Returned via common ATSCAN:
C          PREVFQ        I     FQ id of the previous scan.
C          PREVSU        I     SU id of the previous scan.
C          TSCAN2        D     Scan end time (days).
C          VSCAN2        I     Index of the last visibility.
C
C     Returned via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C                              10  -> No good but file not open yet
C                              other -> No good and output file open
C
C     Called:
C          ATLOD:  {ADJDES, ATHI, FIXCHN, CHNPOL, IFMACH, POLDEC,
C                   SETSSC, TAPHDR}
C          APLSUB: {CATIO, CHCOMP, CHR2H, COPY, DATDAT, H2CHR, HICREQ,
C                   MAKOUT, MSGWRT, TABIO, UVCREA, UVINIT}
C          APLNOT: {NDXINI, TABNDX}
C          APLGEN: {ZEXPND, ZOPEN, ZPHFIL}
C
C     Algorithm:
C
C     Notes:
C       1) An AIPS-byte is half an INTEGER, 16 bits on a VAX or CONVEX.
C
C       2) Definition of the variables FREQ, RA, and DEC in RPFITS.INC
C          in association with the PARAM common preclude the use of
C          routine UVPGET since DUVH.INC also defines these variables
C          in association with common UVHDR.
C
C       3) The AIPS user's output file specification (OUTNAM, OUTCLS,
C          and OUTSEQ) are reloaded into FILNAM, FILCLS, and FILSEQ
C          for each tape file.  If OUTNAM, OUTCLS, and OUTSEQ were
C          fully specified by the user successive files on tape will
C          be concatenated.  Otherwise, separate files will result.
C
C
C-----------------------------------------------------------------------

      INCLUDE 'INCS:PUVD.INC'

      LOGICAL   DOAPND, EQUAL, EXCL, MAP, WAIT
      INTEGER   BUFSZ, FREQID, HDRI(256), I, SOUID, IERR, INXRNO,
     *          IWORK(256), J, JERR, K, NAXIS, NBYPR, NVIS,
     *          NXBUFF(512), NXKOLS(MAXNXC), NXLUN, NXNUMV(MAXNXC),
     *          NXPND, NXVER, SUBARR, IFRQ, ISRC, BO
      REAL      DTIME, HDRR(256), TIME, REFPIX
      HOLLERITH HDRH(256)
      DOUBLE PRECISION HDRD(128), FRQNCY, FRQINC, RAHED, DECHED,
     *   XBLKS, XBYPR, XBPS

      CHARACTER CHTMP*8, DEFCLA*6, DEFNAM*12, OBSDAT*12,
     *          OBJHED*12
      INTEGER POLREF, NUMPOL, POLINC, TRUPL2, IDUM(3), NIF,
     *          NUMCHN

      EQUIVALENCE (HDRI, HDRR, HDRH, HDRD)

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'

      DATA MAP, EXCL, WAIT /.TRUE., .TRUE., .TRUE./
C-----------------------------------------------------------------------
C  Initialize.
      BO = 1
      UVLUN = 16
      HILUN = 27
      NXLUN = 43

C     Fish out parameters from the FIRST matched frequency. These are
C     used to create the header of the output file. The frequency
C     matching was done in HEDHNT.  Other matched frequencies which are
C     incompatible with the first selected one (e.g. Stokes or number
C     of channels will be discarded later)
C
C     Polarizations
      IFRQ = SELIF(1,1)
      CALL POLDEC (IF_NSTOK(IFRQ), IF_CSTOK(1,IFRQ), .TRUE., NUMPOL,
     *             POLREF, POLINC, TRUPOL, IERR)
      IF (IERR.NE.0) THEN
         IERR = 10
         GOTO 999
      END IF

C     Frequencies
      IFRQ = SELIF(1,1)
      NCHAN = IF_NFREQ(IFRQ)
      FRQNCY = IF_FREQ(IFRQ)
      IF (NCHAN.EQ.1) THEN
         FRQINC = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))/NCHAN
      ELSE
         FRQINC = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))/(NCHAN-1)
      END IF
      REFPIX = IF_REF(IFRQ)


C     IF axis length.  Find the simultaneous group of frequencies with
C     the largest number of frequencies and use that, unless the
C     use has specified it.
      IF (NUMIF.LE.0) THEN
         NUMIF = 1
         DO 20 IFRQ = 1, MAX_IF

C           Use selected frequencies only
            IF (SELAX(IFRQ).NE.-1) THEN

C              Get polarizations for this frequency
               CALL POLDEC (IF_NSTOK(IFRQ), IF_CSTOK(1,IFRQ), .FALSE.,
     *                      IDUM(1), IDUM(2), IDUM(3), TRUPL2, IERR)
               IF (IERR.NE.0) THEN
                  IERR = 10
                  GOTO 999
               END IF

C              Update only if number of channels and polarizations same
C              as for the first select frequency
               IF (IF_NFREQ(IFRQ).EQ.NCHAN .AND. TRUPOL.EQ.TRUPL2)
     *             NUMIF = MAX(NUMIF,SELAX(IFRQ))
            END IF
 20      CONTINUE
      END IF
      NIF = NUMIF

C     Are the buffers big enough ?
      IF (NCHAN*NUMPOL.GT.MAXDAT) THEN
         MSGTXT = 'SETUV: TOO MANY CHANNELS & POLARIZATIONS'//
     *            ' FOR INTERNAL STORAGE'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF

C     Fish out source parameters from the first matched source
      ISRC = SELSRC(1)
      RAHED = SU_RA(ISRC)
      DECHED = SU_DEC(ISRC)
      OBJHED = SU_NAME(ISRC)

C     Modify polarization axis if converting to Stokes or
C     kludging as circulars
      CALL CHNPOL (DORRLL, TRUPOL, DOSTOK, YGNROT, POLREF,
     *             POLINC, NUMPOL)

C     Now deal with channel selection
      CALL FIXCHN (MAXDAT, XCHNSL, NCHAN, NUMPOL, NCHNSL, CHANSL,
     *             WCHNSL, BCHAN, ECHAN, NUMCHN)

C     Adjust a variety of axis descriptors to account for channel
C     selection
      CALL ADJDES (BCHAN, NCHNSL, CHANSL, REFPIX, NUMCHN, FRQINC)

C     Set tolerance and expected value for sampler statistics checks.
C     Delayed until now to get date of observation.
      CALL SETSSC (JULSSC, JULSS1, JULSS2, TRUPOL, SSCHK, DATOB8,
     *             XCPARM(7), SSTOL, SSEXP)

C     Set AIPS header for this file
      CALL TAPHDR (FRQNCY, FRQINC, REFPIX, NIF, RAHED, DECHED,
     *             NUMCHN, NUMPOL, POLREF, POLINC, OBJHED(1:8))

C     Make axis increments non-zero.
C     DO 65 I = 0, CATBLK(KIDIM)-1
      DO 65 I = 0, CATBLK(KIDIM)-1
         IF (CATR(KRCIC+I).EQ.0.0 .AND. CATBLK(KINAX+I).EQ.1)
     *    CATR(KRCIC) = 1.0
 65   CONTINUE

C     Create output file name.
      DEFNAM = OBJHED
C      CALL H2CHR (8, 1, CATH(KHOBJ), DEFNAM)
      IF (DEFNAM.EQ.' ') DEFNAM = 'RPFITS'
      DEFCLA = 'UVDATA'

      FILNAM = OUTNAM
      FILCLS = OUTCLS
      FILSEQ = OUTSEQ
      CALL MAKOUT (DEFNAM, DEFCLA, 0, DEFCLA, FILNAM, FILCLS, FILSEQ)
      CALL CHR2H (12, FILNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, FILCLS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = FILSEQ
      CALL CHR2H (2, 'UV', KHPTYO, CATR(KHPTY))

C     Message to user.
      CALL H2CHR (8, 1, CATH(KHDOB), CHTMP)
      CALL DATDAT (CHTMP, OBSDAT)

C     Check PCOUNT.
      IF (CATBLK(KIPCN).GT.20) THEN
         WRITE (MSGTXT, 70) CATBLK(KIPCN)
 70      FORMAT ('SETUV: FOUND',I4,' RANDOM PARAMETERS, ONLY 20 ',
     *      'ALLOWED.')
         CALL ATMSG (MSGTXT)
         IERR = 1
         GO TO 999
      END IF

C     Run a few checks on the data format.
      DO 80 I = 1, CATBLK(KIDIM)
         J = KHCTP + 2*(I-1)

C        Complex axis.
         CALL H2CHR (8, 1, CATH(J), CHTMP)
         IF (CHTMP.EQ.'COMPLEX ') THEN
            IF (I.NE.1) THEN
               WRITE (MSGTXT, 75) I
 75            FORMAT ('SETUV: COMPLEX DATA IS NOT ALLOWED ON AXIS',
     *            I3)
               CALL ATMSG (MSGTXT)
               IERR = 1
               GO TO 999
            ELSE IF (CATBLK(KINAX).GT.3) THEN
C              Non-standard Complex.
               WRITE (MSGTXT, 77) CATBLK(KINAX)
 77            FORMAT ('SETUV: NON-STANDARD COMPLEX AXIS WITH',I3,
     *            ' POINTS.')
               CALL ATMSG (MSGTXT)
               IERR = 1
               GO TO 999
            END IF
            GO TO 80
         END IF
 80   CONTINUE
      CATBLK(KIIMU) = NLUSER


C  Now the necessary steps to create and initialize the uv file.
C     Determine the number of values in a group.
      NAXIS  = CATBLK(KIDIM)
      NAXVAL = 1
      DO 90 I = 1, NAXIS
         NAXVAL = NAXVAL * CATBLK(KINAX+I-1)
 90   CONTINUE

C     Total values per record on disk.
      LREC = CATBLK(KIPCN) + NAXVAL

C     Number of uncompressed values in a group
      NAXVL2 = NAXVAL
      IF (DOUVCM) NAXVL2 = NAXVL2 * 3

C     Length of record when uncompressed
      LREC2 = 7 + NAXVL2

C     Number of AIPS-bytes per record.
      NBYPR = LREC * 2

C     Determine initial space allocation.
      NVIS  = MIN(1000, NBPS*(1000 - 2)/NBYPR)
      NVIS  = MAX(100, NVIS)
      XBLKS = NVIS
      XBYPR = NBYPR
      XBPS = NBPS
      XBLKS = XBLKS * XBYPR + XBPS - 1.0D0
      XBLKS = XBLKS / XBPS + 2.0D0
      NBLKS = XBLKS + 0.001D0
      CATBLK(KIGCN) = NVIS
      VISCNT = 0

C     Assume that a new file will be created.
      DOAPND = .FALSE.
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)

C     Create the uv file.  UVCREA uses values stored in CATBLK.
      CALL UVCREA (OUTDSK, CNO, IWORK, IERR)
      FILSEQ = CATBLK(KIIMS)
      IF (IERR.EQ.2) THEN
C        An old file exists, get its catalogue header.
         CALL CATIO ('READ', OUTDSK, CNO, HDRI, 'WRIT', IWORK, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'SETUV: CATALOGUE READ ERROR.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF

C        Check that the random parameters are consistent.
         IF (HDRI(KIPCN).NE.CATBLK(KIPCN)) IERR = 1
         DO 100 I = 1, CATBLK(KIPCN)
            J = KHPTP + 2*(I-1)
            IF (INSTRUMENT.EQ.'ATCA' .AND. I.LE.3) THEN

C              For the first three axes (UU-L, VV-L and WW-L) make
C              sure we don't check the second half of the string and
C              force it to be NCP for the ATCA
               CALL CHCOMP (4, 1, HDRH(J), 1, CATBLK(J), EQUAL)
C
C              Set NCP strings regardless of what input file has
               CALL CHR2H (4, '-NCP', 5, HDRH(J))
            ELSE
               CALL CHCOMP (8, 1, HDRH(J), 1, CATBLK(J), EQUAL)
            END IF
            IF (.NOT.EQUAL) IERR = 1
 100     CONTINUE
         IF (IERR.NE.0) THEN
            MSGTXT = 'SETUV: CANNOT APPEND FILES, ' //
     *               'RANDOM PARAMETERS ARE INCONSISTENT.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF

C        Check that the regular axes are consistent.
         IF (HDRI(KIDIM).NE.CATBLK(KIDIM)) IERR = 1
         DO 110 I = 1, CATBLK(KIDIM)
C           Be very conservative.
            J = I - 1
            K = 2*J
            CALL CHCOMP (8, 1, HDRH(KHCTP+K), 1, CATBLK(KHCTP+K), EQUAL)
            IF (.NOT.EQUAL) IERR = 1
            IF (HDRI(KINAX+J).NE.CATBLK(KINAX+J)) IERR = 1

C           The following axes don't have to agree exactly.
            CALL H2CHR (8, 1, CATH(KHCTP+K), CHTMP)
            IF (CHTMP.EQ.'FREQ' .OR. CHTMP.EQ.'RA' .OR.
     *          CHTMP.EQ.'DEC') GO TO 110

C           Coordinate reference value, pixel, and increment must agree.
            IF (HDRD(KDCRV+J).NE.CATD(KDCRV+J)) IERR = 1
            IF (HDRR(KRCIC+J).NE.CATR(KRCIC+J)) IERR = 1
            IF (HDRR(KRCRP+J).NE.CATR(KRCRP+J)) IERR = 1
 110     CONTINUE
         IF (IERR.NE.0) THEN
            MSGTXT = 'SETUV: CANNOT APPEND FILES, ' //
     *               'REGULAR AXES ARE INCONSISTENT.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF

C        Check other header values.
         CALL CHCOMP (8, 1, HDRH(KHBUN), 1, CATBLK(KHBUN), EQUAL)
         IF (.NOT.EQUAL) IERR = 1
         IF (IERR.NE.0) THEN
            MSGTXT = 'SETUV: CANNOT APPEND FILES, ' //
     *               'HEADER INCONSISTENCY.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF

C        OK to append to old file.
         DOAPND = .TRUE.

C        Copy the old catalogue header into the new.
         CALL COPY (256, HDRI, CATBLK)

C        Update the output file with the new header as we need to
C        ensure the output file has NCP strings for ATCA
         CALL CATIO ('UPDT', OUTDSK, CNO, CATBLK, 'REST', IWORK, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'SETUV: CATALOGUE UPDATE ERROR.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF
      ELSE IF (IERR.NE.0) THEN
C        Some other problem.
         GO TO 999
      END IF

C     Set the reference frequency.
      REFREQ = CATD(KDCRV+2)

C     TB sorted?
CHM      CALL H2CHR (2, 1, CATBLK(KITYP), CHTMP)
      CALL H2CHR (2, 1, CATH(KITYP), CHTMP)
      TBSORT = CHTMP(1:2).EQ.'TB'

C     Open the uv file.
      CALL ZPHFIL ('UV', OUTDSK, CNO, 1, UVNAME, IERR)
      CALL ZOPEN (UVLUN, UVFIND, OUTDSK, UVNAME, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) GO TO 999
      NUVOPN = NUVOPN + 1

      IF (DOAPND) THEN
C        Expand the output file.
         NXPND = MIN(1000, NBPS*(1000 - 2)/NBYPR)
         NXPND = MAX(100, NXPND)
         XBLKS = NXPND * XBYPR + XBPS - 1.0D0
         XBLKS = XBLKS / XBPS + 1.0D0
         NBLKS = XBLKS
         CALL ZEXPND (UVLUN, OUTDSK, UVNAME, NBLKS, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'SETUV: FAILED TO EXPAND UV FILE FOR APPENDING.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF

         VISCNT = CATBLK(KIGCN)
         NVIS   = VISCNT + NXPND
         XBLKS = NVIS
         XBYPR = NBYPR
         XBPS = NBPS
         XBLKS = XBLKS * XBYPR + XBPS - 1.0D0
         XBLKS = XBLKS / XBPS + 2.0D0
         NBLKS = XBLKS + 0.001D0
         CATBLK(KIGCN) = NVIS
      END IF

C     Initialize uv writing system
      BUFSZ = MAXOUT * 2
      NPIO = 0
      NVIS = 1000000
      CALL UVINIT ('WRIT', UVLUN, UVFIND, NVIS, VISCNT, LREC, NPIO,
     *   BUFSZ, UVOUT, BO, UVBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      NIO = 0

C     Tell common we have a uv file.
      NCFILE  = 1
      FVOL(1) = OUTDSK
      FCNO(1) = CNO
      FRW(1)  = 2


C  Additional steps for TB sorted data.
      IF (TBSORT) THEN
C        Initialize indexes.
         PREVFQ = -1
         PREVSU = -1
         TSCAN2 = -999.D0
         VSCAN2 = VISCNT

         IF (DOAPND) THEN
C           Open the old NX file.
            NXVER = 1
            CALL NDXINI ('READ', NXBUFF, OUTDSK, CNO, NXVER, CATBLK,
     *         NXLUN, INXRNO, NXKOLS, NXNUMV, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'SETUV: ERROR OPENING THE NX TABLE.'
               CALL ATMSG (MSGTXT)
               TBSORT = .FALSE.
               IERR = 0
               GO TO 999
            END IF

C           Find the time of the last recorded visibility.
            INXRNO = NXBUFF(5)
            CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, TIME,
     *         DTIME, SOUID, SUBARR, VSCAN1, VSCAN2, FREQID, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 120) IERR
 120           FORMAT ('SETUV: TABIO ERROR',I3,' READING NX FILE.')
               CALL ATMSG (MSGTXT)
               TBSORT = .FALSE.
               IERR = 0
               GO TO 999
            END IF

            PREVFQ = FREQID
            PREVSU = SOUID
            TSCAN2 = TIME + DTIME/2.0

C           Close the NX table.
            CALL TABIO ('CLOS', 0, 0, NXBUFF, NXBUFF, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'SETUV: ERROR CLOSING NX (SCAN INDEX) TABLE.'
               CALL ATMSG (MSGTXT)
               IERR = 0
               GO TO 999
            END IF
         END IF
      END IF


C  Create and write a HIstory file.
      CALL HICREA (HILUN, OUTDSK, CNO, CATBLK, HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATHI: ERROR CREATING HISTORY FILE.'
         CALL ATMSG (MSGTXT)
         IERR = 1
         GO TO 999
      END IF
      DOHIST = .TRUE.

C     Record adverb values.
      IF (NUVOPN.EQ.1) THEN
         CALL ATHI (JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'SETUV: ERROR INITIALIZING THE HISTORY FILE.'
            CALL ATMSG (MSGTXT)
         END IF
      END IF

C     Make sure multiple input files concatenate
      OUTNAM = FILNAM
      OUTCLS = FILCLS
      OUTSEQ = FILSEQ

 999  RETURN
      END


      SUBROUTINE CHNPOL (DORRLL, TRUPOL, DOSTOK, YGNROT, POLREF,
     *                   POLINC, NUMPOL)
C-----------------------------------------------------------------------
C     Change the label of the poalrization axis if converting to
C     Stokes or kludging as circulars
C
C   Input:
C     DORRLL   L    Kludge to circulars ?
C     TRUPOL   I    Code for input polarizations in data
C                     1: XX
C                     2: YY
C                     3: XX YY
C                     4: XX YY XY YX
C                     0: something else
C   Input/output:
C     DOSTOK   L    Convert to STokes ?
C     YGNROT   L    Rotate Y gain by XY phase ?
C     POLREF   I    Reference value of polarization axis
C     POLINC   I    Polarization axis increment
C     NUMPOL   I    Number of polarizations
C
C-----------------------------------------------------------------------
      INTEGER TRUPOL, POLINC, POLREF, NUMPOL
      LOGICAL DOSTOK, YGNROT, DORRLL
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (DOSTOK .OR. YGNROT) THEN

C        Change polarization axis here if converting to Stokes or
C        rotating the Y gains by the XY phase
         IF (TRUPOL.EQ.3 .OR. TRUPOL.EQ.4) THEN
            IF (DOSTOK) THEN
               MSGTXT = 'Converting polarizations to Stokes parameters.'
               CALL ATMSG (MSGTXT)
               POLREF = 1
               POLINC = 1
               IF (TRUPOL.EQ.3) NUMPOL = 1
            ELSE IF (YGNROT) THEN
               POLINC = -1
               MSGTXT = 'The XY phase will be applied to the Y gains'
               IF (DORRLL) THEN
                  POLREF = -1
                  MSGTXT =
     *               'The XY phase will be applied to the Y gains &'
                  CALL ATMSG (MSGTXT)
                  IF (TRUPOL.EQ.3) THEN
                     MSGTXT = 'Kludging polarization order as RR,LL'
                  ELSE
                     MSGTXT =
     *                  'Kludging polarization order as RR,LL,RL,LR'
                  END IF
                  CALL ATMSG (MSGTXT)
               ELSE
                  POLREF = -5
                  MSGTXT = 'The XY phase will be applied to the Y gains'
                  CALL ATMSG (MSGTXT)
               END IF
            END IF
         ELSE
            MSGTXT = 'Insufficient linear polarizations in these'
            CALL ATMSG (MSGTXT)
            IF (DOSTOK) THEN
               MSGTXT = 'data - abandoned conversion to '//
     *                  'Stokes parameters.'
               CALL ATMSG (MSGTXT)
               DOSTOK = .FALSE.
            ELSE IF (YGNROT) THEN
               MSGTXT = 'data - abandoned Y gain rotation by XY phases'
               CALL ATMSG (MSGTXT)
               YGNROT = .FALSE.
            END IF
         END IF
      ELSE IF (DORRLL) THEN
         POLINC = -1
         POLREF = -1
C
         IF (TRUPOL.EQ.0) THEN
            MSGTXT = 'Kludging polarizations to be circular'
         ELSE IF (TRUPOL.EQ.1) THEN
            MSGTXT = 'Kludging polarization order as RR'
         ELSE IF (TRUPOL.EQ.2) THEN
            MSGTXT = 'Kludging polarization order as LL'
            POLREF = -2
         ELSE IF (TRUPOL.EQ.3) THEN
            MSGTXT = 'Kludging polarization order as RR,LL'
         ELSE IF (TRUPOL.EQ.4) THEN
            MSGTXT = 'Kludging polarization order as RR,LL,RL,LR'
         END IF
         CALL ATMSG (MSGTXT)
      END IF
C
      RETURN
      END



      SUBROUTINE FIXCHN (MAXDAT, XCHNSL, NCHAN, NUMPOL, NCHNSL,
     *                   CHANSL, WCHNSL, BCHAN, ECHAN, NUMCHN)
C-----------------------------------------------------------------------
C     Interpret the CHANSEL array as specified by the user and
C     prepare the length of the frequency axis
C
C  Input:
C    MAXDAT   I      Maximum number of channels we can fit in buffer
C    XCHNSL   R      USe specified channel selection
C    NCHAN    I      The number of channels in the RPFITS data for
C                    the selected frequencies (compatibility checked)
C    NUMPOL   I      The number of polarizations in the OUTPUT file
C                    This is important as Stokes conversion from
C                    XX,YY -> I changes the number of polarizations.
C  Output:
C    NCHNSL   I      Number of groups of channels
C    CHANSL   I      Groups of channel
C    WCHNSL   I      Weight for each channel
C    BCHAN    I      First channel to physically load
C    ECHAN    I      Last channel to physically load
C    NUMCHN   I      Number of channels to physically load (assuming
C                    CHNSL(3)=1
C
C-----------------------------------------------------------------------
      INTEGER NCHAN, NUMPOL, NCHNSL, CHANSL(3,10), BCHAN, ECHAN,
     *        NUMCHN, MAXDAT
      REAL XCHNSL(3,10), WCHNSL(MAXDAT)
CC
      INTEGER I, J, K, CHBEG
C-----------------------------------------------------------------------
C     Intepret users CHANSEL channel selection and find channel
C     range encompassing selected ones
      BCHAN = NCHAN
      ECHAN = 1
      CALL FILL (30, 0, CHANSL)
      NCHNSL = 0
      DO 35 I = 1, 10
         CHANSL(1,I) = XCHNSL(1,I) + 0.5
         CHANSL(2,I) = XCHNSL(2,I) + 0.5
         CHANSL(3,I) = XCHNSL(3,I) + 0.5
         IF (CHANSL(1,I).LE.0 .AND. CHANSL(2,I).LE.0 .AND.
     *       CHANSL(3,I).LE.0) THEN
            IF (I.EQ.1) THEN
               BCHAN = 1
               ECHAN = NCHAN
               NCHNSL = 1
               CHANSL(1,I) = 1
               CHANSL(2,I) = NCHAN
               CHANSL(3,I) = 1
            END IF
            GOTO 40
         ELSE
            NCHNSL = NCHNSL + 1
            IF (CHANSL(1,I).LT.1 .OR. CHANSL(1,I).GT.NCHAN)
     *          CHANSL(1,I) = 1
            IF (CHANSL(2,I).LT.1 .OR. CHANSL(2,I).LT.CHANSL(1,I) .OR.
     *          CHANSL(2,I).GT.NCHAN) CHANSL(2,I) = NCHAN
            IF (CHANSL(3,I).LE.0) CHANSL(3,I) = 1
            BCHAN = MIN (BCHAN, CHANSL(1,I))
            ECHAN = MAX (ECHAN, CHANSL(2,I))
         END IF
 35   CONTINUE

C     Fill weights array with -1 for unselected channels
 40   CALL RFILL (MAXDAT, -1.0, WCHNSL)

C     Loop over each group of channels
      DO 50 I = 1, NCHNSL

C        Loop over selected channels in this group
         DO 45 J = CHANSL(1,I), CHANSL(2,I), CHANSL(3,I)

C           Give each polarization for this channel weight = 1
            CHBEG = (J-1)*NUMPOL + 1
            DO 42 K = CHBEG, CHBEG + NUMPOL - 1
               WCHNSL(K) = 1.0
 42         CONTINUE
 45      CONTINUE
 50   CONTINUE

C     Work out number of channels in output
      NUMCHN = ECHAN - BCHAN + 1
C
      RETURN
      END


      SUBROUTINE ADJDES (BCHAN, NCHNSL, CHANSL, REFPIX, NUMCHN, FRQINC)
C-----------------------------------------------------------------------
C     Channel selection may cause us to modify the axis header
C     description from that of the raw data.  Fix them up here.
C
C  Input:
C   BCHAN     I    First channel to load
C   NCHNSL    I    Number of channel groups
C   CHANSL    I    CHannel groups
C  Input/Output:
C   NUMCHN    I    The number of channels in the output file
C   REFPIX    R    The channel reference pixel
C   FRQINC    D    The frequency increment
C
C-----------------------------------------------------------------------
      INTEGER BCHAN, NCHNSL, CHANSL(3), NUMCHN

      DOUBLE PRECISION FRQINC
      REAL REFPIX
CC
      INTEGER I
C-----------------------------------------------------------------------
C     In the special case of one CHANSEL group only,  we allow the
C     increment to be other than 1 and physically drop the unwanted
C     data. So we need to adjust a few things in this case
      IF (NCHNSL.EQ.1 .AND. CHANSL(3).NE.1) THEN

C        Find total number of loaded channels in output file
         NUMCHN = 0
         DO 50 I = CHANSL(1), CHANSL(2), CHANSL(3)
            NUMCHN = NUMCHN + 1
 50      CONTINUE

C        Modify channel separation (really need a spot for the
C        channel separation and channel width)
         FRQINC = FRQINC * CHANSL(3)

C        Modify reference pixel so first output channel centred
C        on first input channel
         REFPIX = (REFPIX - REAL(CHANSL(1)))/REAL(CHANSL(3)) + 1.0
      ELSE
         REFPIX = REFPIX - BCHAN + 1
      END IF
C
      RETURN
      END


      SUBROUTINE SETSSC (JULSSC, JULSS1, JULSS2, TRUPOL, SSCHK, DATOB8,
     *                   XSSTOL, SSTOL, SSEXP)
C-----------------------------------------------------------------------
C     Set up sampler statistics checks and corrections
C
C  INput:
C    TRUPOL    I     COde for raw polarizations in data
C    JULSSC    D     Sampler corrections done in correlator after
C                    this date
C    JULSS1    D     Julian day before which can't make sampler checks
C                    after which sampler stats should be 17.1 % until
C    JULSS2    D     JULSS2 when they shouyld be 17.3%  Grrrrrr.
C  Input/Output:
C    SSCHK     I     COde for sampler statistic checks
C    DATOB8    C     Date of observation as YYYYMMDD
C    XSSTOL    R     USer specified sampler statistics tolerancwe
C  Output:
C    SSTOL     R     Tolerance for accepting sampler statistics
C    SSEXP     R     Expected value of low and higher sampler stats.
C                    17.1 % after JULSS1, 17.3 after JULSS2
C-----------------------------------------------------------------------
      INTEGER TRUPOL, SSCHK
      REAL XSSTOL, SSTOL, SSEXP
      CHARACTER DATOB8*8
      DOUBLE PRECISION JULSS1, JULSS2, JULSSC
CC
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION JDAY0
C-----------------------------------------------------------------------
      IF (SSCHK.GT.0) THEN

C        Julian date at the start of the observation.

         CALL JULDAY (DATOB8, JDAY0)

C        Sampler corrections done in correlator after this date
         IF (JDAY0.GE.JULSSC) THEN
            IF (SSCHK.EQ.4) THEN
               MSGTXT = 'Sampler corrections are done in correlator now'
               CALL ATMSG (MSGTXT)
               MSGTXT = 'Disabling request for sampler corrections'
               CALL ATMSG (MSGTXT)
               SSCHK = 0
               GOTO 999
            END IF
         END IF

C        Can't correct samplers if input data converted to
C        Stokes on-line
         IF (SSCHK.EQ.4 .AND. TRUPOL.EQ.0) THEN
            MSGTXT = 'The input data have been converted to Stokes'
            CALL ATMSG (MSGTXT)
            MSGTXT = 'Sampler statistics error corrections disabled'
            CALL ATMSG (MSGTXT)
            SSCHK = 0
            GOTO 999
         END IF

C
         IF (JDAY0.GE.JULSS1) THEN

C           Set expected values of low and high samplers
C
            IF (JDAY0.GE.JULSS2) THEN
               SSEXP = 17.3
            ELSE
               SSEXP = 17.1
            END IF

C           Set tolerances.  12% for correction, 5% for dropping
C           without correction.  Use user value if given
            SSTOL = 5.0
            IF (SSCHK.EQ.4) SSTOL = 12.0
            IF (XSSTOL.NE.0.0) SSTOL = XSSTOL

C           Don't let them be idiots
            IF (SSTOL.GT.12.0 .AND. SSCHK.EQ.4) THEN
               SSTOL = 12.0
               MSGTXT ='SETSSC: Setting sampler tolerance above 12% not'
               CALL ATMSG (MSGTXT)
               MSGTXT = 'SETSSC: allowed when correcting for '//
     *                  'bad statistics'
               CALL ATMSG (MSGTXT)
            END IF
C
            IF (SSCHK.EQ.4) THEN
               WRITE (MSGTXT, 10) SSEXP, SSTOL
 10            FORMAT ('Data dropped if samplers outside of ',
     *                 'range ', F4.1, ' +/- ' , F4.1, '%')
               CALL ATMSG (MSGTXT)
               MSGTXT = 'Data within this range are corrected'
               CALL ATMSG (MSGTXT)
            ELSE IF (SSCHK.EQ.3 .OR. SSCHK.EQ.2) THEN
               WRITE (MSGTXT, 10) SSTOL
               CALL ATMSG (MSGTXT)
            ELSE IF (SSCHK.EQ.1) THEN
               WRITE (MSGTXT, 20) SSEXP, SSTOL
 20             FORMAT ('Warnings issued if samplers outside of ',
     *                 'range ', F4.1, ' +/- ' , F4.1, '%')
               CALL ATMSG (MSGTXT)
            END IF
         ELSE
            MSGTXT = 'Data too old for sampler statistics checks'
            CALL ATMSG (MSGTXT)
            SSCHK = 0
         END IF
      END IF
C
 999  RETURN
      END



      SUBROUTINE TAPHDR  (FRQNCY, FRQINC, REFPIX, NIF, RAHED, DECHED,
     *                    NUMCHN, NUMPOL, POLREF, POLINC, OBJHED)
C-----------------------------------------------------------------------
C     TAPHDR updates an AIPS catalog header with information found
C     in the RPFITS header.
C
C     Given via common ATCTRL:
C          DOUVCM        L     If true use compressed uv data format.
C
C     Given via RPFITS common PARAM
C          COORD         C*8   Coordinate system for RA and DEC, should
C                              should be J2000
C     Given
C          FRQNCY        D     Frequency, in Hz.
C          FRQINC        D     Frequency increment between spectral
C                              channels, in Hz.
C          REFPIX        R     Reference pixel
C          NIF           I     Length of IF axis
C          RA,DECHED     R     RA and DECof first selected source
C          NUMCHN        I     Number of channels in output
C          NUMPOL        I     Number of polarizations in output
C          POLREF        I     Rerefence value of polarization axis
C          POLINC        I     Increment on polarization axis
C          OBJHED        C*8   Object name
C
C     Given via RPFITS common NAMES:
C          INSTRUMENT    C*16  One of
C                                 ATCA    Australia Telescope Compact
C                                         Array
C                                 ATLBA   Australia Telescope Long
C                                         Baseline Array
C                                 ATLBAN
C                                 ATCAN
C                                 PTI     Parkes-Tidbinbilla
C                                         Interferometer
C                                 DUMMY   (for debugging)
C          RP_OBSERVER   C*16  Name of the observer(s).
C          DATOBS        C*12  UT date of observation in the form
C                              yyyy-mm-dd
C
C
C     Given via RPFITS common SPECT:
C          IVELREF       I     Velocity reference = 256i + j where
C                                 i=0: optical definition
C                                 i=1: radio definition
C                                 j=0: wrt LSR
C                                 j=1: wrt sun
C                                 j=2: wrt observer
C          RFREQ         D     Line rest frequency, in Hz.
C          VEL1          D     Velocity of channel 1, in m/s.
C
C     Returned via common MAPHDR:
C          CATBLK(256)   I     Catalog header block.
C
C
C     Called:
C          APLSUB: {CHR2H}
C          APLSOL: {UDATE}
C
C     Algorithm:
C
C     Notes:
C       1) This needs to be generalized.
C
C       2) In the case where the last spectral channel contains T(sys)
C          the central spectral channel and the channel increment must
C          be adjusted.  This applies only to early observations (1989).
C
C
C-----------------------------------------------------------------------

      INTEGER   NUMPOL, POLREF, POLINC, NIF, NUMCHN, IOS
      CHARACTER CHTMP*8, OBJHED*8
      CHARACTER DATTMP*12
      DOUBLE PRECISION FRQNCY, FRQINC, RAHED, DECHED
      REAL REFPIX

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
C  Fill in the image catalogue header.
      CALL CHR2H (8, OBJHED, 1, CATH(KHOBJ))
      CALL CHR2H (8, INSTRUMENT, 1, CATH(KHTEL))
      CALL CHR2H (8, RP_OBSERVER, 1, CATH(KHOBS))
C      type *,'TAPHDR: writing DATOB8 ',DATOB8, 'to catalog'
      CALL CHR2H (8, DATOB8, 1, CATH(KHDOB))

      CALL CHR2H (8, 'JY      ', 1, CATH(KHBUN))
      IF (INSTRUMENT.EQ.'ATCA') THEN
         CALL CHR2H (8, 'UU-L-NCP', 1, CATH(KHPTP))
         CALL CHR2H (8, 'VV-L-NCP', 1, CATH(KHPTP+2))
         CALL CHR2H (8, 'WW-L-NCP', 1, CATH(KHPTP+4))
      ELSE
         CALL CHR2H (8, 'UU-L', 1, CATH(KHPTP))
         CALL CHR2H (8, 'VV-L', 1, CATH(KHPTP+2))
         CALL CHR2H (8, 'WW-L', 1, CATH(KHPTP+4))
      END IF
      CALL CHR2H (8, 'BASELINE', 1, CATH(KHPTP+6))
      CALL CHR2H (8, 'TIME1   ', 1, CATH(KHPTP+8))
      CALL CHR2H (8, 'SOURCE  ', 1, CATH(KHPTP+10))
      CALL CHR2H (8, 'FREQSEL ', 1, CATH(KHPTP+12))
      CALL CHR2H (8, 'COMPLEX ', 1, CATH(KHCTP))
      CALL CHR2H (8, 'STOKES  ', 1, CATH(KHCTP+2))
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
      CALL CHR2H (8, 'IF      ', 1, CATH(KHCTP+6))
      CALL CHR2H (8, 'RA      ', 1, CATH(KHCTP+8))
      CALL CHR2H (8, 'DEC     ', 1, CATH(KHCTP+10))

      CATD(KDCRV)   = 1.0
      CATD(KDCRV+1) = POLREF
      CATD(KDCRV+2) = FRQNCY
      CATD(KDCRV+3) = 1.0

      CATD(KDCRV+4) = RAHED*R2D
      CATD(KDCRV+5) = DECHED*R2D
C
      CATR(KRCIC)   = 1.0
      CATR(KRCIC+1) = POLINC
      CATR(KRCIC+2) = FRQINC
      CATR(KRCIC+3) = 1.0
      CATR(KRCIC+4) = 1.0
      CATR(KRCIC+5) = 1.0
      CATR(KRCRP)   = 1.0
      CATR(KRCRP+1) = 1.0
      CATR(KRCRP+2) = REFPIX
      CATR(KRCRP+3) = 1.0
      CATR(KRCRP+4) = 1.0
      CATR(KRCRP+5) = 1.0
      CATR(KREPO)   = 0.0
      READ (COORD(2:8), *, IOSTAT=IOS) CATR(KREPO)
      CATR(KRBLK)   = 0.0
      CATBLK(KIPCN)   = 7
      CATBLK(KIDIM)   = 6
      CATBLK(KINAX)   = 3
      CATBLK(KINAX+1) = NUMPOL

      CATBLK(KINAX+2) = NUMCHN
      CATBLK(KINAX+3) = NIF
      CATBLK(KINAX+4) = 1
      CATBLK(KINAX+5) = 1
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
      CATBLK(KIALT)   = IVELREF
      CATD(KDRST)     = RFREQ
      CATD(KDARV)     = VEL1
      CATR(KRARP)     = REFPIX


C     Get today's UT date as yyyy-mm-dd, convert to yyyymmdd.
      CALL UDATE (DATTMP)
      CALL DATFST ('F2L',DATTMP)
      CHTMP = DATTMP
      CALL CHR2H (8, CHTMP, 1, CATH(KHDMP))

C     Use compressed uv data format?
      IF (DOUVCM) THEN
         CALL CHR2H (8, 'WEIGHT  ', 1, CATH(KHPTP+14))
         CALL CHR2H (8, 'SCALE   ', 1, CATH(KHPTP+16))
         CATBLK(KIPCN) = 9
         CATBLK(KINAX) = 1
      END IF


      RETURN
      END



      SUBROUTINE SYSINI
C----------------------------------------------------------------------
C     SYSINI initializes system parameter arrays.
C
C     Parameters set by ATLOD.INC:
C          NCA, MAXSFQ, MAXSSF, MAXSSU
C
C     Returned via common ATSYSC:
C          NSYSPH(NCA,MAXSSF,MAXSFQ)
C                        I     Number of XY phase accumulations for each
C                              antenna in time series so far.  Gets
C                              reset on occaision.  If this is zero,
C                              then there is no SYSCAL info available
C                              yet.
C          NSYSTX(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        I     Number of X "temperatures" accumulated
C                              for ANT, FREQID, IF & SOURCE so far.
C          NSYSTY(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        I     Number of Y "temperatures" accumulated
C                              for ANT, FREQID, IF & SOURCE so far.
C          PHSAVE(NCA,MAXSSF,MAXSFQ)
C                        R     Average of XY phase stacks.  Phases are
C                              accumulaed for each antenna frequency,
C                              and IF.
C          TXAVE(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        R     Average of X Tsys stacks.
C          TYAVE(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        R     Average of Y Tsys stacks.
C                              System temperatures are accumulated for
C                              each antenna, frequency, and source.
C-----------------------------------------------------------------------

      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
CC
      INTEGER   IANT, IFQ, IIF, ISU
C-----------------------------------------------------------------------
C     Initialize.

      DO 40 IFQ = 1, MAXSFQ
         DO 30 IIF = 1, MAXSSF
            DO 20 IANT = 1, NCA
               NSYSPH(IANT,IIF,IFQ) = 0
               PHSAVE(IANT,IIF,IFQ) = 0.0
 20         CONTINUE
 30      CONTINUE
 40   CONTINUE

      DO 70 ISU = 1, MAXSSU
         DO 60 IFQ = 1, MAXSFQ
            DO 50 IIF = 1, MAXSSF
               DO 45 IANT = 1, NCA
                  NSYSTX(IANT,IIF,IFQ,ISU) = 0
                  NSYSTY(IANT,IIF,IFQ,ISU) = 0
C
                  TXAVE(IANT,IIF,IFQ,ISU)  = 0.0
                  TYAVE(IANT,IIF,IFQ,ISU)  = 0.0
 45            CONTINUE
 50         CONTINUE
 60      CONTINUE
 70   CONTINUE

      RETURN
      END



      SUBROUTINE ATLIST (IFILE, NHDR, FIRST, TOTVIS, TOTFLG, EOF, IERR)
C-----------------------------------------------------------------------
C     ATLIST reports what's in the scan and accumulates grand totals.
C
C     Given:
C          IFILE         I     Number of file being read.
C          NHDR          I     Number of headers read from file.
C
C     Given via common ATCTRL:
C          LISLEV        I     List level
C                                 0: file summary
C                                 1: scan summary
C
C     Given and returned:
C          FIRST         L     If true then this is the first time in
C                              here.
C          TOTVIS        I     Total number of unflagged visibilities in
C                              file.
C          TOTFLG        I     Total numner of flagged visibilities in
C                              file.
C
C     Returned:
C          EOF           L     True if EOF reached.
C          IERR          I     Error status
C                                 0: encountered EOF, a header, FG
C                                    table, end of scan, or illegal
C                                    data.  Next action is to (re)read
C                                    a header or next file
C                                 1: error that forces us to stop
C
C     Called:
C        RPFITS: {RPFITSIN}
C        ATLOD:  {RDTOCH, TIMCON}
C        APLSUB: {JULDAY, MSGWRT}
C
C
C-----------------------------------------------------------------------

      INCLUDE 'RPFITS.INC'

      LOGICAL   FIRST, EOF, MORE, NODATA, FSTVIS
      INTEGER   IFILE, IFRQ, IERR, ILEN, ISRC, JSTAT, JTRIM, K,
     *          NFLAGD(MAX_SU,MAX_IF), NHDR, NSUVIS(MAX_SU,MAX_IF),
     *          NSYSCL, T1(4), T2(4), TLEN, TOTFLG, TOTVIS
      DOUBLE PRECISION  JDAY, JDAY0
      CHARACTER STRING*72, STKSTR*11
      INTEGER   BAD1

      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ATLOD.INC'

      SAVE JDAY0
C-----------------------------------------------------------------------
C     File name and scan number.
      TLEN = JTRIM (FILE)
      WRITE (MSGTXT, 5) IFILE, FILE(1:TLEN), NHDR
 5    FORMAT ('File',I3,' = ',A,'    Scan',I4)
      CALL ATMSG (MSGTXT)

C     Match user specified frequencies
      IF (IF_FOUND) THEN
         CALL IFMACH (IERR)
         IF (IERR.NE.0) GOTO 999
      ELSE
         MSGTXT = 'ATLIST: NO FREQUENCY TABLE FOUND FOR THIS SCAN, ' //
     *            'SKIPPING IT.'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF


C     Match user specified sources
      IF (SU_FOUND) THEN
         CALL SUMACH (IERR)
         IF (IERR.NE.0) GOTO 999
      ELSE
         MSGTXT = 'ATLIST: NO SOURCE TABLE FOUND FOR THIS SCAN, '//
     *            'SKIPPING IT'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF


C     Proceed if there are some selected frequencies and sources
      IF (NIFSEL*NSUSEL.EQ.0) THEN
         MSGTXT = 'ATLIST: no frequencies and/or sources '//
     *            'selected for this scan'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF

C     Julian date at the start of the observation or for current scan.
      IF (FIRST) THEN
         CALL JULDAY (DATOB8, JDAY0)
         FIRST = .FALSE.
      END IF
      CALL JULDAY (DATOB8, JDAY)

C     Initialize counters
      NODATA = .TRUE.
      NSYSCL = 0
      DO 20 IFRQ = 1, MAX_IF
         DO 10 ISRC = 1, MAX_SU
            NSUVIS(ISRC,IFRQ) = 0
            NFLAGD(ISRC,IFRQ) = 0
 10      CONTINUE
 20   CONTINUE

C     Loop over visibilities until we exhaust data.
      MORE = .TRUE.
      FSTVIS = .TRUE.
 30   IF (MORE) THEN
C        Read the next visibility.
         JSTAT = 0
         CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W, FLAG,
     *      BIN, IFNUM, SRCNUM)
         IERR = 0
         IF (JSTAT .NE. 5) THEN
            BAD1 = 0
         END IF
         IF (JSTAT.NE.0) THEN
            MORE = .FALSE.
            IF (JSTAT.EQ.-1) THEN
               IERR = 1
            ELSE IF (JSTAT.EQ.1) THEN
               IF (NODATA) THEN
C                 A header has been encountered, but we have had no data
C                 since the last header.  The next header read by READHD
C                 will be the one we just encountered.
                  IF (LISLEV.GT.0) THEN
                     MSGTXT = '*******************************' //
     *                        '*******************************'
                     CALL ATMSG (MSGTXT)
                     WRITE (MSGTXT, 40) IFILE, FILE(1:TLEN), NHDR
 40                  FORMAT ('File',I3,' = ',A,'    Scan',I4)
                     CALL ATMSG (MSGTXT)
                     MSGTXT = 'This scan contains no data.'
                     CALL ATMSG (MSGTXT)
                     MSGTXT = ' '
                     CALL ATMSG (MSGTXT)
                  END IF
                  GO TO 999
               END IF
            ELSE IF (JSTAT.EQ.3) THEN
               EOF = .TRUE.
            ELSE IF (JSTAT.EQ.5) THEN
               IF (BAD1 .EQ. 0) THEN
C                 Ignor JSTAT 5 first time. Read next record.
                  BAD1 = 1
                  MORE = .TRUE.
                  GO TO 30
               ENDIF
C              Sequential jstat=5 records. Must really be bad data.
               MSGTXT = 'ATLIST: INVALID DATA, IGNORING THE REST OF ' //
     *                  'THE SCAN.'
               CALL ATMSG (MSGTXT)
            END IF
            GO TO 60
         END IF

C        Convert from UT to offset time
         NODATA = .FALSE.
         IF (FSTVIS) THEN
            TSCAN1 = (JDAY - JDAY0) + UT/86400D0
         ELSE
            TSCAN2 = (JDAY - JDAY0) + UT/86400D0
         END IF
         FSTVIS = .FALSE.

C        Check for system calibration
         IF (BASELN.EQ.-1) THEN
            NSYSCL = NSYSCL + 1
         ELSE IF (BASELN.GT.256) THEN
C           Check for flagged data.
            IF (FLAG.NE.0) THEN
               NFLAGD(SRCNUM,IFNUM) = NFLAGD(SRCNUM,IFNUM) + 1
               TOTFLG = TOTFLG + 1
            ELSE
               NSUVIS(SRCNUM,IFNUM) = NSUVIS(SRCNUM,IFNUM) + 1
               TOTVIS = TOTVIS + 1
            END IF
         ELSE
C           Something is wrong with this visibility, issue warning.
            WRITE (MSGTXT, 50)
 50         FORMAT ('WARNING! Unrecognizable baseline, your data may ',
     *              'be corrupt.')
            CALL ATMSG (MSGTXT)
         END IF
         GO TO 30
      END IF

C     Return from here if file summary requested.
 60   IF (LISLEV.EQ.0) GO TO 999

C     Convert scan start and end times to something palatable.
      CALL TIMCON (TSCAN1, T1)
      CALL TIMCON (TSCAN2, T2)

C  List summary.
      MSGTXT =
     * '**************************************************************'
      CALL ATMSG (MSGTXT)

C     Observer etc.
      WRITE (MSGTXT, 80) INSTRUMENT, RP_OBSERVER
 80   FORMAT ('Instrument: ',A,'   Observer: ',A)
      CALL ATMSG (MSGTXT)

C     Dates and times.
      WRITE (MSGTXT, 90) DATOBS, T1, T2
 90   FORMAT ('Date: ',A,'  UT start:',I3,I3.2,':',I2.2,':',I2.2,
     *        '  end:',I3,I3.2,':',I2.2,':',I2.2)
      CALL ATMSG (MSGTXT)
      MSGTXT = ' '
      IF (LISLEV.GT.0) CALL ATMSG (MSGTXT)

C     Sideband indicator.
      DO 100 IFRQ = 1, NNIF
         WRITE (MSGTXT, 95) IF_FREQ(IFRQ)/1.0E9, IF_INVERT(IFRQ)
 95      FORMAT ('Sideband indicator at ', F8.4, ' GHz is ', SP, I3)
         CALL ATMSG (MSGTXT)
 100  CONTINUE


C     System calibration records.
      WRITE (MSGTXT, 110) NSYSCL
 110  FORMAT ('System calibration records:',I4)
      CALL ATMSG (MSGTXT)
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)

C     Source name, frequency, band width, no. vis and flagged vis.
      DO 150 ISRC = 1, NNSU
         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         CALL ATMSG (MSGTXT)
         CALL RDTOCH (R2D, SU_RA(ISRC), SU_DEC(ISRC), STRING, ILEN)
         MSGTXT = 'Source: ' // SU_NAME(ISRC) // '  ' // STRING(1:ILEN)
         CALL ATMSG (MSGTXT)
         MSGTXT = ' '
         CALL ATMSG (MSGTXT)

         MSGTXT = 'channels frequency   bandwidth    '//
     *            'Stokes      visibilities'
         CALL ATMSG (MSGTXT)
         MSGTXT = '            GHz         MHz             ' //
     *            '      valid    flagged'
         CALL ATMSG (MSGTXT)
         MSGTXT = '----------------------------------------' //
     *            '-----------------------'
         CALL ATMSG (MSGTXT)

         DO 140 IFRQ = 1, NNIF
            WRITE (STKSTR, 120) (IF_CSTOK(K, IFRQ),K=1,IF_NSTOK(IFRQ))
 120        FORMAT (3(A2,1X),A2)
            WRITE (MSGTXT, 130) IF_NFREQ(IFRQ), IF_FREQ(IFRQ)/1.0E9,
     *         IF_BW(IFRQ)/1.0E6, STKSTR, NSUVIS(ISRC,IFRQ),
     *         NFLAGD(ISRC,IFRQ)
 130        FORMAT (I5,F12.4,F11.1,2X,A,I8,I10)
            CALL ATMSG (MSGTXT)
 140     CONTINUE
 150  CONTINUE

      MSGTXT = ' '
      CALL ATMSG (MSGTXT)


 999  RETURN
      END



      SUBROUTINE SCANIN (TTY, DOTCLS, IFILE, ISCAN, EOF, IERR)
C-----------------------------------------------------------------------
C     SCANIN processes the next scan, writing the data to the AIPS UV
C     file and updating the FQ, SU, NX, and CL tables.
C
C     Given:
C          TTY(2)        I     Terminal I/O buffer
C          IFILE         I     File number of current file.
C          ISCAN         I     Scan number of current scan.
C
C     Given via common ATUVIO:
C          TBSORT        L     True if the data is in time sequence.
C
C     Returned:
C          DOTCLS        L     True if terminal is yet to be closed
C                              when listing SYSCAL info.
C          EOF           L     True if EOF reached.
C          IERR          I     Error status, 0 means success.
C
C     Called:
C          ATLOD:  {ATANT, ATFQIN, ATMSG, ATSUIN, ATUV, IFMACH,
C                   REPORT, SUMACH}
C          APLSUB: {MSGWRT}
C
C     Notes:
C       1) If SCANIN returns with NFQ=0 or NSU=0, the next call MUST be
C          to READHD.
C
C
C-----------------------------------------------------------------------

      INCLUDE 'RPFITS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ATLOD.INC'
C
      INTEGER MAXREJ
      PARAMETER (MAXREJ = 14)

      LOGICAL   EOF, DOTCLS
      INTEGER  IERR, IFILE, ISCAN, JERR,
     *   NREJCT(MAXREJ,MAX_IF), NSUVIS(MAX_SU,MAX_IF,2),
     *   TTY(2), NZFILL(MAX_IF), NSSFIX(MAX_IF),
     *   NPHCLP(ANT_MAX,MAX_IF), NTXCLP(ANT_MAX,MAX_IF),
     *   NTYCLP(ANT_MAX,MAX_IF)
      REAL SSSUM(MAX_IF), SSMIN(MAX_IF), SSMAX(MAX_IF)

C-----------------------------------------------------------------------
C
C     Process the scan header.
C
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)
      WRITE (MSGTXT, 10) IFILE, ISCAN
 10   FORMAT ('File:',I3,',  Scan:',I4)
      CALL ATMSG (MSGTXT)

C     Match user specified frequencies
      IF (IF_FOUND) THEN
         CALL IFMACH (IERR)
         IF (IERR.NE.0) GOTO 999
      ELSE
         MSGTXT = 'SCANIN: NO FREQUENCY TABLE FOUND FOR THIS SCAN, ' //
     *            'SKIPPING IT.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


C     Match user specified sources
      IF (SU_FOUND) THEN
         CALL SUMACH (IERR)
         IF (IERR.NE.0) GOTO 999
      ELSE
         MSGTXT = 'SCANIN: NO SOURCE TABLE FOUND FOR THIS SCAN, '//
     *            'SKIPPING IT'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF


C     Proceed if there are some selected frequencies and sources
      IF (NIFSEL*NSUSEL.EQ.0) THEN
         MSGTXT = 'SCANIN: no frequencies and/or sources '//
     *            'selected for this scan'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF

C     Find translation between frequency and FREQID and fill FQ table
      CALL ATFQIN (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SCANIN: ERROR READING THE FQ (FREQUENCY) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


C     Translate between source name and SOURCE ID and fill source table
      CALL ATSUIN (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SCANIN: ERROR READING THE SU (SOURCE ID) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Update the AN table if necessary.
      IF (AN_FOUND) THEN
         CALL ATANT (IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'SCANIN: ERROR UPDATING THE AN (ANTENNA) FILE.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         ELSE IF (NANT.GT.NCA) THEN
            WRITE(MSGTXT, 15) NANT, NCA
  15        FORMAT('SCANIN: No of antennae (',I3,') exceeds NCA (',I3,
     *             ') - recompile!')
            CALL ATMSG (MSGTXT)
            NSEVER = MIN(NSEVER+1, MAXSEV)
            SEVERE(NSEVER) = MSGTXT
            GO TO 999
         END IF
      END IF


C  Read in data for this scan and write to AIPS file.
      CALL ATUV (TTY, DOTCLS, NSUVIS, MAXREJ,
     *           NREJCT, NZFILL, NSSFIX, SSSUM, SSMIN,
     *           SSMAX, NPHCLP, NTXCLP, NTYCLP, JERR)
      IF (JERR.GT.0) THEN
C        Fatal error.
         MSGTXT = 'SCANIN: ERROR PROCESSING VISIBILITIES.'
         CALL ATMSG (MSGTXT)
         IERR = 1
         GO TO 999
      END IF

C     Tell user what was read in from this scan
      CALL REPORT (NSUVIS, MAXREJ, NREJCT, NZFILL, NSSFIX,
     *             SSSUM, SSMIN, SSMAX)

C     Test for end-of-file returned from ATUV.
      IF (JERR.EQ.-1) THEN
         EOF = .TRUE.
         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         WRITE (MSGTXT, 20) IFILE
 20      FORMAT ('Finished reading file number', I3,'.')
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


 999  RETURN
      END



      SUBROUTINE ATMSG (TEXT)
C-----------------------------------------------------------------------
C     ATMSG writes informative messages to the user and history file
C     together.
C
C     Given:
C          TEXT          C*80  The message.
C
C     Given via common ATUVIO:
C          HILUN         I     HI file logical unit number.
C          DOHIST        L     True if history file is open and
C                              ready for text.
C
C     Given and returned via common ATUVIO:
C          HIBUFF(512)   I     History buffer.
C
C     Returned:
C          none
C
C     Called:
C          APLSUB: {HIADD, MSGWRT}
C
C     Notes:
C       1) ATMSG may only be called AFTER the history file has been
C          initialized by routine ATHI which is called by SETUV.
C
C       2) If possible, the message text should be limited to 66
C          characters otherwise it will be truncated in the history
C          output.
C
C
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER HILINE*72, TEXT*80

      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
C     Message file first.
      MSGTXT = TEXT
      CALL MSGWRT (8)

C     Then the history file.
      IF (DOHIST) THEN
         HILINE = 'ATLOD ' // TEXT
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      END IF


      RETURN
      END



      SUBROUTINE GRAND (IFILE, NHDR, FILE, TOTVIS, TOTFLG)
C-----------------------------------------------------------------------
C     GRAND reports some grand totals from all scans in the file.  For
C     'LIST' option only.
C
C     Given:
C          IFILE         I     File number.
C          NHDR          I     Number of scans examined.
C          FILE          C     Input file name.
C          TOTVIS        I     Total number of valid visibilities.
C          TOTFLG        I     Total number of flagged visibilities.
C
C-----------------------------------------------------------------------
      INTEGER   IFILE, JTRIM, NHDR, TLEN, TOTVIS, TOTFLG
      CHARACTER FILE*(*)

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------

      MSGTXT = ' '
      CALL ATMSG (MSGTXT)

      TLEN = JTRIM (FILE)
      WRITE (MSGTXT, 10) IFILE, FILE(1:TLEN)
 10   FORMAT ('Totals from file',I3,': ',A)
      CALL ATMSG (MSGTXT)

      WRITE (MSGTXT, 20) NHDR, TOTVIS, TOTFLG
 20   FORMAT ('Scans:',I4,', visibilities (valid/flagged)',I6,'/',I6)
      CALL ATMSG (MSGTXT)

      RETURN
      END



      SUBROUTINE ATFEOF (NHDR, EOF, IERR)
C-----------------------------------------------------------------------
C     ATFEOF skips to the end-of-file and closes an RPFITS tape file.
C
C     Given via common CHRCOM:
C          INFILE        C*48  Disk file containing RPFITS data (if
C                              any).
C
C     Given
C          NHDR          I     Number of scans read so far in this file
C     Given and returned:
C          EOF           L     Is the file positioned at eof?
C
C     Returned:
C          IERR          I     Error status
C                                 0: success
C                                 1: failed to skip to eof
C                                 2: failed to close file
C
C     Called:
C          ATLOD:  {ATMSG}
C          RPFITS: {RPFITSIN}
C          APLSUB: {MSGWRT}
C
C     Algorithm:
C
C     Notes:
C       1) RPFITSIN offers the option of skipping to end-of-file but
C          doesn't say how far it had to skip.
C
C
C-----------------------------------------------------------------------
      LOGICAL   EOF
      INTEGER   IERR, JSTAT, NRECRD, ISCAN, NHDR

      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
C     First skip to end-of-file if reading from tape.
C
      IF (INFILE.EQ.' ') THEN
         NRECRD = 0
         ISCAN  = 0
         JSTAT  = 0
 10      IF (.NOT.EOF) THEN
            CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W,
     *       FLAG, BIN, IFNUM, SRCNUM)
            IF (JSTAT.LT.0) THEN
C              Unrecoverable error.
               MSGTXT = 'ATFEOF: ERROR ADVANCING TO EOF.'
               CALL ATMSG (MSGTXT)
               IERR = 1
               GO TO 30
            ELSE IF (JSTAT.EQ.0) THEN
C              Got a data group.
               NRECRD = NRECRD + 1
            ELSE IF (JSTAT.EQ.1) THEN
C              Encountered the next header, read past it.
               ISCAN = ISCAN + 1
               WRITE (MSGTXT, 15) NHDR+ISCAN
 15            FORMAT ('Skipped scan ', I4, ' searching for EOF')
               CALL ATMSG (MSGTXT)
               JSTAT = -1
            ELSE IF (JSTAT.EQ.3) THEN
C              End-of-file at last.
               EOF = .TRUE.
            END IF
            GO TO 10
         END IF

         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         IF (NRECRD.GT.0) THEN
            WRITE (MSGTXT, 20) ISCAN, NRECRD
 20         FORMAT ('Skipped',I4,' scans (',I6,
     *              ' records) to end-of-file.')
            CALL ATMSG (MSGTXT)
         END IF
      END IF


C     Now close the file.
C
 30   JSTAT = 1
      IERR = 0
      CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W, FLAG,
     *   BIN, IFNUM, SRCNUM)
      IF (JSTAT.NE.0) THEN
         MSGTXT = 'ATFEOF: ERROR CLOSING THE RPFITS FILE.'
         CALL ATMSG (MSGTXT)
         IERR = 2
      END IF


      RETURN
      END



      SUBROUTINE ATCLOS (IERR)
C-----------------------------------------------------------------------
C     ATCLOS closes the UV file, flushing and compressing it if
C     necessary, and updating the number of visibilities (groups)
C     recorded in its catalogue header.  It also closes the history
C     file.
C
C     Given via common ATCTRL:
C          FILSEQ        I     Actual output uv sequence number.
C          OUTDSK        I     Output disk number.
C
C     Given via common CHRCOM:
C          FILNAM        C*12  Actual output uv file name.
C          FILCLS        C*6   Actual output uv class.
C
C     Given via common ATUVIO and ATUVCH:
C          CNO           I     UV file catalogue slot number.
C          UVNAME        C*48  AIPS UV file name.
C          UVLUN         I     UV file logical unit number.
C          UVFIND        I     FTAB UV file pointer returned by ZOPEN.
C          LREC          I     Number of values in a visibility record.
C          UVBUFF(*)     R     UV IO buffer.
C          UVBIND        I     Pointer into UVBUFF.
C          NIO           I     Number of visibilities to be written.
C          VISCNT        I     Total number of visibilities in the UV
C                              file.
C          HILUN         I     HI file logical unit number.
C
C     Given and returned via common ATUVIO:
C          HIBUFF(512)   I     History buffer.
C
C     Returned via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          APLSUB: {CATDIR, CATIO, HICLOS, MSGWRT, UVDISK}
C          APLGEN: {ZCLOSE, ZCMPRS}
C
C     Algorithm:
C
C     Notes:
C       1)
C
C
C-----------------------------------------------------------------------
      LOGICAL   UPDATE
      INTEGER   IERR, IOBLK(256), LSIZE, BSIZE

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
C  Wind up uv disk input/output, and update the uv catalog header.
C     Flush the last uv buffer.
      NIO = -NIO
      CALL UVDISK ('FLSH', UVLUN, UVFIND, UVOUT, NIO, UVBIND, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATCLOS: ERROR FLUSHING THE AIPS UV DISK FILE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Compress the uv file.
      IF (CATBLK(KIGCN).GT.VISCNT) THEN
C        Add NBPS for safety.
         LSIZE = VISCNT*LREC*2 + NBPS
         BSIZE = (LSIZE - 1) / 512 + 1
         CALL ZCMPRS (OUTDSK, UVNAME, UVLUN, BSIZE, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ATCLOS: ERROR COMPRESSING THE AIPS UV DISK FILE.'
            CALL ATMSG (MSGTXT)
            GO TO 999
         END IF
      END IF

C     Update the number of groups in the uv catalog header.
      CATBLK(KIGCN) = VISCNT
      CALL CATIO ('UPDT', OUTDSK, CNO, CATBLK, 'REST', IOBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATCLOS: ERROR UPDATING GROUP COUNT IN THE UV ' //
     *      'CATALOG HEADER.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Tell user something about weights
      IF (NWT.GT.0.0) THEN
         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         WRITE (MSGTXT, 100) AVGWT/NWT
 100     FORMAT ('The average visibility weight was ', F6.3)
         CALL ATMSG (MSGTXT)
      END IF

C     Close the history file.
      UPDATE = .TRUE.
      CALL HICLOS (HILUN, UPDATE, HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATCLOS: ERROR CLOSING THE AIPS HISTORY FILE.'
         CALL ATMSG (MSGTXT)
      ELSE
         DOHIST = .FALSE.
      END IF

C     Close the uv file.
      CALL ZCLOSE (UVLUN, UVFIND, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATCLOS: ERROR CLOSING THE AIPS UV DISK FILE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Clear the catalogue WRIT status.
      CALL CATDIR ('CSTA', OUTDSK, CNO, FILNAM, FILCLS, FILSEQ, 'UV',
     *   NLUSER, 'CLWR', IOBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATCLOS: ERROR CLEARING UV FILE WRITE STATUS.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Update CFILES common.
      NCFILE = 0


 999  RETURN
      END



      SUBROUTINE RPOPEN (IERR)
C-----------------------------------------------------------------------
C     RPOPEN opens the RPFITS file.
C
C     Returned:
C          IERR          I     Error status, 0 means success.
C
C     Called:
C          RPFITS: {RPFITSIN}
C          APLSUB: {MSGWRT}
C
C
C-----------------------------------------------------------------------
      INTEGER IERR, JSTAT
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
      JSTAT = -3
      IERR = 0

      CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W, FLAG,
     *   BIN, IFNUM, SRCNUM)
      IF (JSTAT.EQ.-1) THEN
         MSGTXT = 'ERROR, UNABLE TO OPEN THE RPFITS FILE.'
         CALL ATMSG (MSGTXT)
         IERR = 1
      END IF

      RETURN
      END



      SUBROUTINE READHD (NHDR, IERR)
C-----------------------------------------------------------------------
C     READHD reads the next RPFITS header returning values via the
C     RPFITS common blocks.
C
C     Given:
C          NHDR          I     Number of headers read so far
C
C     Returned via RPFITS common PARAM:
C          FREQ          D     Frequency, in Hz.
C
C     Returned via RPFITS common NAMES:
C          OBJECT        C*16  Source name for this scan.
C
C     Returned via RPFITS common SU:
C          SU_NAME(MAX_SU)
C                        C*16  Source names.
C
C     Returned:
C          IERR          I     Error status
C                               -1  End-of-file
C                                0  Header read successfully
C                                1  Flag table read
C                                2  Error of some sort
C
C     Called:
C          RPFITS: {RPFITSIN}
C          APLSUB: {MSGWRT, DATFST}
C          LOCAPLNOT: {UPCASE}
C
C     Notes:
C       1) All source names are converted to upper case here.
C
C
C-----------------------------------------------------------------------

      INTEGER IERR, ISRC, JSTAT, NHDR
      CHARACTER DATTMP*12

      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
      IERR    = 0
      JSTAT   = -1
      NCARD   = 1
      CARD(1) = 'FORMAT'

C     Open RPFITS file
      CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W, FLAG,
     *   BIN, IFNUM, SRCNUM)
      IF (JSTAT.EQ.0) THEN
C        Check for RPFITS format.
         IF (CARD(1)(25:30).NE.'RPFITS'.AND.
     *       CARD(1)(24:29).NE.'RPFITS') THEN
            MSGTXT = 'THIS FILE IS NOT IN RPFITS FORMAT.'
            CALL ATMSG (MSGTXT)
            IERR = 1
            GO TO 999
         END IF
      ELSE IF (JSTAT.EQ.3) THEN
C        End-of-file read.
         MSGTXT = 'End-of-file read.'
         CALL ATMSG (MSGTXT)
         IERR = -1
         GO TO 999
      ELSE IF (JSTAT.EQ.4) THEN
C        Flag table read.
         MSGTXT = 'Encountered RPFITS flag table.'
         CALL ATMSG (MSGTXT)
         IERR = 1
         GO TO 999
      ELSE
C        Some problem reading scan header.
         MSGTXT = 'READHD: ERROR READING SCAN HEADER.'
         CALL ATMSG (MSGTXT)
         IERR = 2
         GO TO 999
      END IF
      NHDR = NHDR + 1

C     Change object name to upper case.
      CALL UPCASE (OBJECT)

      IF (SU_FOUND) THEN
         DO 10 ISRC = 1, NNSU
            CALL UPCASE (SU_NAME(ISRC))
 10      CONTINUE
      END IF

C     Convert dates from fits header
C                     from new fits form YYYY-MM-DD
C                     or   old fits form DD/MM/YY
C                     to local AIPS form YYYYMMDD in common DATES
      DATTMP = DATOBS
      CALL DATFST('F2L', DATTMP)
      DATOB8 = DATTMP
      DATTMP = DATWRIT
      CALL DATFST('F2L', DATTMP)
      DATWR8 = DATTMP

 999  RETURN
      END
      SUBROUTINE ATHI (IERR)
C-----------------------------------------------------------------------
C     ATHI builds a history file from the ATLOD adverbs and RPFITS
C     history.
C     Given via parameters in ATLOD.INC:
C          ATLODD, ATLODV
C     Given via common ATCTRL:
C          INTAPE        I     Input tape drive number.
C          NSKIP         I     Number of files to skip
C          NFILES        I     Number of files to load.
C          BSCAN         I     First scan to load (same for all files).
C          ESCAN         I     Last scan to load.  Big number if user
C                              doesn't specify
C          NSCANS        I     Number of scans to load (same for all
C                              files).
C          OUTSEQ        I     Output sequence number supplied by user.
C          FILSEQ        I     Actual output uv sequence number.
C          OUTDSK        I     Output disk number.
C          DOLIST        L     If true, just list a summary of the file
C                              and don't load it.
C          LISLEV        I     List level
C                                 0: file summary
C                                 1: scan summary
C          NSOURC        I     Number of sources in source list.
C          DOSWNT        L     If true, sources are selected, otherwise
C                              rejected.
C          TIMRNG(2)     R     Time range selected, days.
C          NFREQ         I     Number of frequencies selected by user
C          FREQS         D     Selected frequencies
C          BCHAN         I     First channel to load
C          DOUVCM        L     If true, use compressed uv data format.
C          DOSTOK        L     If true convert linear polarizations to
C                              Stokes.
C          DORRLL        L     Call the polarizations RR,LL,RL,LR.
C          DROFLG        L     If true drop flagged visibilities .
C          DOAUTO        L     If true, retain autocorrelations.
C          SHADOW        R     Shadowing diameter, in meters.
C          NXGAP         R     Maximum gap before starting a new scan
C                              index (NX) entry, in days.
C          NXSPAN        R     Maximum length of a scan index (NX)
C                              entry, in days.
C          CLSPAN        R     Interval for CL entries, in days.
C          DOTSYS        I     -1  => Undo on-line correction
C                               0  => Do nothing
C                               N  => Undo on-line Tsys and redo
C                                     with N averaged Tsys measurements
C          NSTAKP        I     Number of integration periods to average
C                              XY phases over for Stokes conversion.
C          DOXYFL        L     If true drop visibilties when XY
C                              phase sbad.
C          EXBSLN(NCA,NCA)
C                        L     Baselines to be dropped.
C          PRTSYS        I        1: List system cal info
C                                 2: Warn about accumulator resets
C                                 3: 1 & 2
C          DOSNEG        L     If true, negate system XY cal phases.
C          DONPHA        L     If true, negate visibility phases.
C
C     Given via common CHRCOM:
C          FILNAM        C*12  Actual output uv name.
C          FILCLS        C*6   Actual output uv class.
C          INFILE        C*48  Disk file containing RPFITS data (if
C                              any).
C          OUTNAM        C*12  Output uv name supplied by user.
C          OUTCLS        C*6   Output uv class supplied by user.
C          SOURCS(30)    C*16  Source selection (or rejection) list.
C
C     Given via common ATUVIO:
C          CNO           I     UV file catalogue slot number.
C          HILUN         I     HI file logical unit number.
C          HIBUFF(512)   I     History buffer.
C
C     Given via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Given via RPFITS common NAMES:
C          FILE          C*80  Input file name.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          APLSUB: {HIADD, IROUND}
C
C     Algorithm:
C
C     Notes:
C       1)
C-----------------------------------------------------------------------
      LOGICAL   GOTONE
      INTEGER   I, IA1, IA2, J, JMAX, KMAX, IERR, IROUND, I1, I2, I3
      CHARACTER CHSRC1*18, CHSRC2*18, HILINE*72, SPACER*72, XSRCS(30)*16
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      DATA SPACER /'ATLOD'/
C-----------------------------------------------------------------------
C     Header message in HI file.
      HILINE = 'ATLOD ----------------------------------------------' //
     *   '--------------------'
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      HILINE = 'ATLOD Release ' // ATLODV // ' dated ' // ATLODD //
     *         ' running under ' // RLSNAM // 'AIPS.'
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)


C  Transcribe ATLOD adverbs.
      CALL HIADD (HILUN, SPACER, HIBUFF, IERR)

      HILINE= 'ATLOD   Adverbs  :    Input value   Interpretation'
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 10) XITAPE, INTAPE
 10   FORMAT ('ATLOD   INTAPE   :',F15.5,'   Tape drive number',I3)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 20) XNSKIP, NSKIP
 20   FORMAT ('ATLOD   NSKIP    :',F15.5,'   Skip',I3,' tape files')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 30) XNFILE, NFILES
 30   FORMAT ('ATLOD   NFILES   :',F15.5,'   Load',I3,' tape files')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 40) XBSCAN, BSCAN
 40   FORMAT ('ATLOD   BCOUNT   :',F15.5,'   First scan is',I5)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 50) XNSCAN, NSCANS
 50   FORMAT ('ATLOD   NCOUNT   :',F15.5,'   Read',I6,' scans')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      IF (ESCAN.LT.10000) THEN
         WRITE (HILINE, 51) ESCAN
 51      FORMAT ('ATLOD',31X,'(last scan is',I6,')')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      END IF

      WRITE (HILINE, 60) INFILE
 60   FORMAT ('ATLOD   INFILE   : "',A,'"')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      HILINE = 'ATLOD              "' // FILE
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 70) OUTNAM, FILNAM
 70   FORMAT ('ATLOD   OUTNAM   : "',A,'"   uv file name  ',A)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 80) OUTCLS, FILCLS
 80   FORMAT ('ATLOD   OUTCLS   :       "',A,'"   uv file class ',A)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 90) OUTSEQ, FILSEQ
 90   FORMAT ('ATLOD   OUTSEQ   :',I5,'   uv file seq.',I5)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 100) OUTDSK, OUTDSK
 100  FORMAT ('ATLOD   OUTDSK   :',I5,'   Output disk',I6)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 110) XOPTYP, DOLIST
 110  FORMAT ('ATLOD   OPTYPE   :         "',A4,'"   List only?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      JMAX = 0
      KMAX = 0
      DO 120 J = 1, 30
         CALL H2CHR (16, 1, XSOURC(1,J), XSRCS(J))
         IF (XSRCS(J).NE.' ') JMAX = J
         IF (SOURCS(J).NE.' ') KMAX = J
 120  CONTINUE
      IF (JMAX.EQ.29) JMAX = 30
      IF (KMAX.EQ.29) KMAX = 30

      IF (JMAX.EQ.0) THEN
         HILINE = 'ATLOD   SOURCES  :    (all blank)   All sources ' //
     *            'SELECTED'
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      ELSE
         IF (DOSWNT) THEN
            WRITE (HILINE, 121) NSOURC
 121        FORMAT ('ATLOD   SOURCES  :',I21,' sources SELECTED:')
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
         ELSE
            WRITE (HILINE, 122) NSOURC
 122        FORMAT ('ATLOD   SOURCES  :',I21,' sources REJECTED:')
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
         END IF

         DO 124 J = 1, JMAX
            CHSRC1 = '"' // XSRCS(J)  // '"'
            IF (J.LE.KMAX) THEN
               CHSRC2 = '"' // SOURCS(J) // '"'
            ELSE IF (J.EQ.KMAX+1) THEN
               CHSRC2 = 'rest blank'
            ELSE
               CHSRC2 = ' '
            END IF

            WRITE (HILINE, 123) J, CHSRC1, CHSRC2
 123        FORMAT ('ATLOD      ',I2,')   : ',A,'   ',A)
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
 124     CONTINUE

         IF (JMAX.LT.30) THEN
            HILINE = 'ATLOD                  rest blank'
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
         END IF
      END IF

      WRITE (HILINE, 130) XTIMER
 130  FORMAT ('ATLOD   TIMERANG : ',8F6.1)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      WRITE (HILINE, 131)
 131  FORMAT ('ATLOD',31X,'Time range selection (days):')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      WRITE (HILINE, 132) TIMRNG
 132  FORMAT ('ATLOD',30X,2F13.6)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      IF (NFREQS.EQ.0) THEN
         HILINE = 'ATLOD   FREQSEL  : (all zero)   All frequencies ' //
     *            'SELECTED'
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      ELSE
         WRITE (HILINE, 133) NFREQS
 133     FORMAT ('ATLOD   FREQSEL  :',I4,' frequencies SELECTED:')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
         DO 136 J = 1, NFREQS
            WRITE (HILINE, 134) J, XSELFR(J)
 134        FORMAT ('ATLOD      ',I2,')   : ', F8.2)
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
 136     CONTINUE
      END IF

      DO 147 J = 1, NCHNSL
         WRITE (HILINE, 141) J, XCHNSL(1,J), CHANSL(1,J)
 141     FORMAT ('ATLOD   CHANSEL(1,',I2,') :',F10.5,
     *           '   Start channel',I5)
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
         WRITE (HILINE, 142) J, XCHNSL(2,J), CHANSL(2,J)
 142     FORMAT ('ATLOD   CHANSEL(2,',I2,') :',F10.5,
     *           '   End   channel',I5)
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
         WRITE (HILINE, 143) J, XCHNSL(3,J), CHANSL(3,J)
 143     FORMAT ('ATLOD   CHANSEL(3,',I2,') :',F10.5,
     *           '   Increment    ',I5)
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
 147  CONTINUE

      WRITE (HILINE, 150) XIFMAP, IFMAP
 150  FORMAT ('ATLOD   IFMAP    :',F15.5,
     *        '   Map IF chain to IF axis?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 160) XNIF, NUMIF
 160  FORMAT ('ATLOD   NIF      :',F15.5,'   Length of IF axis',I5)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 170) XDOUVC, DOUVCM
 170  FORMAT ('ATLOD   DOUVCOMP :',F15.5,'   Use compressed uv?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 180) XAPARM(1), DOSTOK
 180  FORMAT ('ATLOD   APARM(1) :',F15.5,'   Convert to Stokes?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      WRITE (HILINE, 181) DORRLL
 181  FORMAT ('ATLOD',31X,'Kludge polarizations?',L3)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 190) XAPARM(2), DROFLG
 190  FORMAT ('ATLOD   APARM(2) :',F15.5,'   Drop flagged data?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 200) XAPARM(3), DOAUTO
 200  FORMAT ('ATLOD   APARM(3) :',F15.5,'   Retain autocorrel?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 210) XAPARM(4), SHADOW
 210  FORMAT ('ATLOD   APARM(4) :',F15.5,'   Shadow diameter',F10.2,
     *   ' m')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 220) XAPARM(5), NXGAP*(24.0*60.0)
 220  FORMAT ('ATLOD   APARM(5) :',F15.5,'   Max NX time gap',F10.2,
     *   ' min')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 230) XAPARM(6), NXSPAN*(24.0*60.0)
 230  FORMAT ('ATLOD   APARM(6) :',F15.5,'   Max NX scan length',F7.2,
     *   ' min')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 240) XAPARM(7), CLSPAN*(24.0*60.0)
 240  FORMAT ('ATLOD   APARM(7) :',F15.5,'   CL entry interval',F8.2,
     *   ' min')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      IF (DOTSYS.EQ.-1) THEN
         WRITE (HILINE, 250) XAPARM(8)
 250     FORMAT ('ATLOD   APARM(8) :',F15.5,
     *           '   Undo on-line Tsys correction')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      ELSE IF (DOTSYS.EQ.0) THEN
         WRITE (HILINE, 251) XAPARM(8)
 251     FORMAT ('ATLOD   APARM(8) :',F15.5,
     *           '   No Tsys corrections made')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      ELSE
         WRITE (HILINE, 252) XAPARM(8)
 252     FORMAT ('ATLOD   APARM(8) :',F15.5,
     *           '   Do off-line Tsys correction')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
         WRITE (HILINE, 253) DOTSYS
 253     FORMAT ('ATLOD',31X,'Tsys stack size',I9)
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      END IF

      WRITE (HILINE, 260) XAPARM(9), NSTAKP
 260  FORMAT ('ATLOD   APARM(9) :',F15.5,'   XY phase stack size',I5)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 270) XAPARM(10), DOXYFL
 270  FORMAT ('ATLOD   APARM(10):',F15.5,'   Drop bad XY-phase?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      GOTONE = .FALSE.
      DO 280 J = 1, 10
         IF (XBPARM(J).NE.0.0) GOTONE = .TRUE.
 280  CONTINUE

      IF (GOTONE) THEN
         WRITE (HILINE, 281) (XBPARM(J), J=1,5)
 281     FORMAT ('ATLOD   BPARM    :',5F7.2)
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
         WRITE (HILINE, 282) (XBPARM(J), J=6,10)
 282     FORMAT ('ATLOD            :',5F7.2)
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

         WRITE (HILINE, 283)
 283     FORMAT ('ATLOD',30X,'Baselines dropped:')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

         WRITE (HILINE, 284) (J, J=1,NCA)
 284     FORMAT ('ATLOD',31X,'I/J',9(I2,:))
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

         DO 287 IA1 = 1, NCA
            WRITE (HILINE, 285) IA1, (EXBSLN(IA1,IA2), IA2=1,NCA)
 285        FORMAT ('ATLOD',31X,I2,1X,9(L2,:))
            DO 286 J = 39, 72
               IF (HILINE(J:J).EQ.'T') THEN
                  HILINE(J:J) = '*'
               ELSE IF (HILINE(J:J).EQ.'F') THEN
                  HILINE(J:J) = '-'
               END IF
 286        CONTINUE
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
 287     CONTINUE
      ELSE
         WRITE (HILINE, 288)
 288     FORMAT ('ATLOD   BPARM    :     (all zero)   ',
     *           'No baselines dropped')
         CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
      END IF

      WRITE (HILINE, 290) XCPARM(1), PRTSYS
 290  FORMAT ('ATLOD   CPARM(1) :',F15.5,'   Syscal print level',I6)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 300) XCPARM(2), DOSNEG
 300  FORMAT ('ATLOD   CPARM(2) :',F15.5,'   Negate XY  phases?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 310) XCPARM(3), DONPHA
 310  FORMAT ('ATLOD   CPARM(3) :',F15.5,'   Negate vis phases?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 320) XCPARM(4), USERXY
 320  FORMAT ('ATLOD   CPARM(4) :',F15.5,'   Use XYPHASE array?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 330) XCPARM(5), YGNROT
 330  FORMAT ('ATLOD   CPARM(5) :',F15.5,'   Apply XY phase?',L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C

      WRITE (HILINE, 335) XCPARM(6), SSCHK
 335  FORMAT ('ATLOD   CPARM(6) :',F15.5,'   Sampler stats check level',
     *         1x, I1)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

C
      WRITE (HILINE, 340) XCPARM(7), SSTOL
 340  FORMAT ('ATLOD   CPARM(7) :',F15.5,'   Sampler stats tolerance',
     *        1x, F5.1)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
      WRITE (HILINE, 345) XCPARM(10), TSYFDG
 345  FORMAT ('ATLOD   CPARM(10):',F15.5,'   Apply Tsys IF 1 to IF 2',
     *        1x, L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
      IF (USERXY) THEN
         DO 400 J = 1, 6
            WRITE (HILINE, 350) (XYPHAS(I,J)*R2D, I=1,6)
 350        FORMAT ('ATLOD   XYPHAS          :', 6(F6.1,','))
            CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
 400     CONTINUE
      END IF
C
      WRITE (HILINE, 405) XDPARM(1), XDPARM(1).GT.0.0
 405  FORMAT ('ATLOD   DPARM(1) :',F15.5,
     *        '   Fudge scan based src ID = 1 ?', 1x, L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
      WRITE (HILINE, 410) XDPARM(2), XDPARM(2).GT.0.0
 410  FORMAT ('ATLOD   DPARM(2) :',F15.5,
     *        '   Hanning smooth spectra ?     ', 1x, L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
C
      WRITE (HILINE, 420) XDPARM(3), XDPARM(3)
 420  FORMAT ('ATLOD   DPARM(3) :',F15.5,
     *        '   Set weights to               ', 1x, F6.3)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      WRITE (HILINE, 430) XDPARM(4), FLGFDG
 430  FORMAT ('ATLOD   DPARM(4) :',F15.5,
     *        '   Apply flagging of IF 1 to IF 2', 1x, L4)
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      I1 = IROUND (XDPARM(5))
      I2 = IROUND (XDPARM(6))
      I3 = IROUND (XDPARM(7))
      WRITE (HILINE, 440) I1, I2, I3
 440  FORMAT ('ATLOD   DPARM(5-7): ',3I5,
     *        ' Pulsar bin selection')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

Crgd >
      I1 = IROUND (XDPARM(10))
      WRITE (HILINE, 450) XDPARM(9), I1
 450  FORMAT ('ATLOD   DPARM(9-10): ',F14.4,I6,
     *        ' Pulsar dedispersion')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)
Crgd <

      I1 = IROUND (XDPARM(8))
      WRITE (HILINE, 460) I1
 460  FORMAT ('ATLOD   DPARM(8) :',I5,
     *        '   Recalculation of U,V,W')
      CALL HIADD (HILUN, HILINE, HIBUFF, IERR)

      RETURN
      END



      SUBROUTINE RDTOCH (R2D, RA, DEC, STRING, ILEN)
C-----------------------------------------------------------------------
C     RDTOCH encodes RA and DEC into a string.
C
C     Given:
C          RA,DEC        D     Right ascension and declination, in
C                              radians.
C
C     Returned:
C          STRING        C**   Encoded string.
C          ILEN          I     Length of string.
C
C     Called:
C          none
C
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION RA, DEC, R2D
      CHARACTER STRING*(*)
      INTEGER ILEN
CC
      DOUBLE PRECISION ADEC, FRAC, ARA
      INTEGER IRA(3), IDEC(3), I, J
      CHARACTER*1 S

C-----------------------------------------------------------------------
C     RA first
      ARA = RA * R2D / 15.0
      IRA(1) = ARA
      FRAC = (ARA - REAL(IRA(1))) * 60.0
      IRA(2) = FRAC
      IRA(3) = NINT((FRAC - REAL(IRA(2))) * 60.0)

C     Now DEC
      IF (DEC.LT.0.0) THEN
         S = '-'
      ELSE
         S = '+'
      END IF

      ADEC = ABS(DEC * R2D)
      IDEC(1) = ADEC
      FRAC = (ADEC - REAL(IDEC(1))) * 60.0
      IDEC(2) = FRAC
      IDEC(3) = NINT((FRAC - REAL(IDEC(2))) * 60.0)

      WRITE (STRING, 10) (IRA(I), I=1,3), S, (IDEC(J), J=1,3)
 10   FORMAT ('RA = ',I2.2,2I3.2,'  DEC = ',A,I2.2,2I3.2)
      ILEN = 31

      RETURN
      END



      SUBROUTINE ATFQIN (IERR)
C-----------------------------------------------------------------------
C     ATFQIN sets the FQTAGS translation table from IF number in
C     the RPFITS IF_* tables to FREQID in the AIPS FQ table.
C
C     Given via common ATCTRL:
C          OUTDSK        I     Output disk number.
C          NCHAN         I     The number of channels for the frequency
C                              used to create the data base, excluding
C                              CHANSEL selection.
C          TRUPOL        I     Code describing polarizations in
C                              RPFITS scan
C
C     Given via common ATUVIO:
C          CNO           I     UV file catalogue slot number.
C          REFREQ        D     Reference frequency, Hz.
C
C     Given via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C
C     Given via RPFITS common IF:
C          NNIF          I     Number of entries in the RPFITS IF table.
C          IFNUMS(MAX_IF)
C                        I     RPFITS IF numbers corresponding to the
C                              IFNUM random parameter for this scan.
C          IF_FREQ(MAX_IF)
C                        D     Reference frequency (usually the centre
C                              frequency) of each band in Hz.
C          IF_REF(MAX_IF)
C                        D     Channel number of the reference
C                              frequency.
C          IF_NFREQ(MAX_IF)
C                        I     Number of spectral channels in each band.
C          IF_NSTOK(MAX_IF)
C                        I     Number of polarizations in each band.
C          IF_CSTOK(4,MAX_IF)
C                        C*2   Type of each polarization (XX, YY, etc)
C
C     Given via common ATSEL:
C          NIFSEL        I     Number of groups of simultaneous
C                              frequencies found in users list
C          SELIF(MAX_IF,MAX_SIM)
C                        I     List of pointers to frequencies in IF
C                              table that user wants for each freq. in
C                              each simultaneous group of frequencies
C          SELNIF(MAX_IF)
C                        I     Number of frequencies in each
C                              simultaneous group
C          SELAX(MAX_IF)
C                        I     Conversion from IFNUM to AIPS IF axis
C
C     Returned via common ATSEL:
C          XYPHPT(MAXSSF,MAXSFQ)
C                        I     FOr a given FREQID and IFAXID, tells
C                              which entry in the user specified
C                              XYPHAS array to use.
C                              Only if USERXY=.TRYUE.
C          FQTAGS(MAX_IF)
C                        I     Translation from frequency identification
C                              number in the RPFITS header to that of
C                              the FQ file.
C                                 -1: frequency is not wanted
C                                  N: this is the FREQID for this IFNUM
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          ATLOD:  {ATMSG}
C          APLSUB: {MSGWRT, TABIO}
C          APLNOT: {FQINI, TABFQ}
C
C     Algorithm:
C
C     Notes:
C        1)   When matching a group of frequencies with the FREQID
C           already in the FQ table, we need to worry about partial
C           matches.  That is, the current group of frequencies
C           (the number in which may not necessarily be the same
C           as the length of the IF axis) may match a subset of
C           the FREQID.  Do these frequencies then get allocated
C           the partially matched FREQID or not ???  The code is
C           present for both cases.
C
C           Also need to be concerned with the case where SCAN 1
C           has one of 2 chains only, then SCAN 2 has both.  Ideally
C           we need to put these into the same FREQID.  User can use
C           NIF=2 to make sure IF axis long enough.    More thinking
C           needed though.
C
C
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'
      INTEGER  IERR

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:PUVD.INC'

      INTEGER   FREQID, FQBUFF(4096), FQKOLS(MAXFQC), FQLUN, TRUPL2,
     *          FQNUMV(MAXIF), FQVER,  IFQRNO, IDUM(3), I, J, K,
     *          IFSIDE(MAXIF), IFRQ, JFRQ, NFRQ, NUMCHN, IFAXID, NLOOP
      REAL      IFCHW(MAXIF), IFTBW(MAXIF), FTOT, REFPIX, REFPIX2
      DOUBLE PRECISION  FRQDIF, IFFREQ(MAXIF), FRQR, FINC
      LOGICAL MATCH, FULMCH
      CHARACTER BNDCOD(MAXIF)*8

      DATA IFSIDE /MAXIF*0/
      DATA IFCHW, IFTBW /MAXIF*0.0, MAXIF*0.0/
      DATA IFFREQ /MAXIF*0.0D0/
C-----------------------------------------------------------------------
      IF (XNIF.GT.0.0) THEN

C        User has specified length of IF axis, so there may be
C        some partial matches between FREQID and simultaneous
C        frequency groups wanted
         FULMCH = .FALSE.
      ELSE

C        ATLOD works out length of IF axis, so demand a full
C        FREQID match
         FULMCH = .TRUE.
      END IF

C     Only do partial matches for now
      FULMCH = .FALSE.


      IERR = 0
      NUMCHN = CATBLK(KINAX+2)

C     Deselect all frequencies.
      DO 10 IFRQ = 1, MAX_IF
         FQTAGS(IFRQ) = -1
 10   CONTINUE

C     Find reference pixel fo AIPS UV file
      REFPIX = CATR(KRCRP+2)

C     Loop over number of selected simultaneous groups and mark
C     those frequencies that we want ready to be assigned a FREQID
      DO 50 I = 1, NIFSEL

C        Loop over number of simultaneous frequencies in this group
         DO 40 J = 1, SELNIF(I)

C           Find RPFITS IF table entry number
            IFRQ = SELIF(I,J)

C           Interpret polarizations for this frequency
            CALL POLDEC (IF_NSTOK(IFRQ), IF_CSTOK(1,IFRQ), .FALSE.,
     *                   IDUM(1), IDUM(2), IDUM(3), TRUPL2, IERR)
            IF (IERR.NE.0) GOTO 999

C           Set translation from IFNUM to FREQID as being wanted
C           if descriptors are consistent with the header values
            IF (TRUPL2.NE.TRUPOL) THEN
               WRITE (MSGTXT, 20) IF_FREQ(IFRQ)/1.0E9
 20            FORMAT ('Drop freq. ', F8.4, ' as polns. do not ',
     *                 'match those in UV file')
               CALL ATMSG (MSGTXT)
            ELSE IF (IF_NFREQ(IFRQ).NE.NCHAN) THEN
               WRITE (MSGTXT, 25) IF_FREQ(IFRQ)/1.0E9
 25            FORMAT ('Drop freq. ', F8.4, ' as no. chans ',
     *                 'not equal to that in UV file')
               CALL ATMSG (MSGTXT)
            ELSE IF (SELAX(IFRQ).GT.NUMIF) THEN
               WRITE (MSGTXT, 30) IF_FREQ(IFRQ)/1.0E9, NUMIF
 30            FORMAT ('Freq. ', F8.4, ' dropped as the IF axis ',
     *                 'is only of length ', I1)
               CALL ATMSG (MSGTXT)

C              Fix the conversion from IFNUM to IF axis to reflect
C              the fact that we don't want this one.
               SELAX(IFRQ) = -1
            ELSE
C              Mark as wanted
               FQTAGS(IFRQ) = 0
            END IF
 40      CONTINUE
 50   CONTINUE


C     Create or reopen the FQ file.
      FQVER = 1
      FQLUN = 42

      CALL FQINI ('WRIT', FQBUFF, OUTDSK, CNO, FQVER, CATBLK, FQLUN,
     *   IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATFQIN: ERROR OPENING THE FQ (FREQUENCY) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Loop through all selected groups of simultaneous frequencies
C     and try to match group in FQ table
      IFQRNO = 0
      NFRQ = FQBUFF(5)
      DO 140 I = 1, NIFSEL

C        Read through the FQ table
         MATCH = .FALSE.
         DO 90 JFRQ = 1, NFRQ
            IFQRNO = JFRQ
            CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *          FREQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT, 60) IERR
 60            FORMAT ('ATFQIN: ERROR',I3,' READING FQ FILE.')
               CALL ATMSG (MSGTXT)
               GO TO 990
            END IF


C           Match group with table entry
            IF (FULMCH) THEN

C              *FULL* match required.
               IF (SELNIF(I).NE.NUMIF) THEN
                  MATCH = .FALSE.
                  GOTO 85
               ELSE
                  NLOOP = NUMIF
               END IF
            ELSE

C              *PARTIAL* match required
               NLOOP = SELNIF(I)
            END IF

C           Try to match this FREQID record with this group of
C           simultaneous frequencies.
            MATCH = .TRUE.
            DO 80 J = 1, NLOOP

C              Find RPFITS IF table entry number; NLOOP is never more
C              than SELNIF(I) so this is a valid statement, but watch
C              out if you change things above.
               IFRQ = SELIF(I,J)

C              Watch out for frequencies that were dropped for
C              incompatibility
               IF (FQTAGS(IFRQ).EQ.0) THEN

C                 Where is this frequency going to go on the IF axis ?
C                 This will be the pointer to the AIPS FQ table array
                  IFAXID = SELAX(IFRQ)

C                 Channel separation
                  IF (NCHAN.LE.1) THEN
                     FINC = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))
                  ELSE
                     FINC = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))/(NCHAN-1)
                  END IF

C                 Total bandwidth of selected channels.  Hope that if
C                 they did Hanning that CHANSL(3,1) = 2 !!
                  FTOT = ABS(FINC) * NUMCHN
                  IF (HANN .AND. NCHNSL.EQ.1) FTOT = FTOT * CHANSL(3,1)

C                 Adjust reference pixel and increment for channel
C                 selection
                  IF (NCHNSL.EQ.1 .AND. CHANSL(3,1).NE.1) THEN
                     FINC = FINC * CHANSL(3,1)
                     REFPIX2 = (IF_REF(IFRQ) -
     *                 REAL(CHANSL(1,1)))/REAL(CHANSL(3,1)) + 1.0
                  ELSE
                     REFPIX2 = IF_REF(IFRQ) - BCHAN + 1
                  END IF

C                 Find the frequency of this scan at the reference pixel
C                 of the AIPS UV file; the RPFITS ref. pix. may vary.
                  IF (REFPIX.NE.REFPIX2) THEN
                     FRQR = IF_FREQ(IFRQ) + (REFPIX - REFPIX2)*FINC
                  ELSE
                     FRQR = IF_FREQ(IFRQ)
                  END IF

C                 Difference between this and AIPS UV file reference
C                 frequency
                  FRQDIF = FRQR - REFREQ

C                 Match the offset frequency, frequency increment
C                 and the total band-width
                  IF (ABS(IFFREQ(IFAXID)-FRQDIF).GT.FRQTOL .OR.
     *                ABS( IFCHW(IFAXID)-FINC)  .GT.FRQTOL/2.0 .OR.
     *                ABS( IFTBW(IFAXID)-FTOT)  .GT.FRQTOL/2.0) THEN
                     MATCH = .FALSE.
                     GO TO 85
                  END IF
               END IF
 80         CONTINUE

C           Bug out if matched
 85         IF (MATCH) GOTO 95

C           Try to match with another FREQID
 90      CONTINUE


C        If we matched this group, assign the FREQID to all RPFITS
C        IFNUMs representing this group
 95      IF (MATCH) THEN
            DO 100 J = 1, SELNIF(I)

C              Find RPFITS IF table entry number
               IFRQ = SELIF(I,J)

C              Enter FREQID into translation table if this
C              frequency is on the wanted list
               IF (FQTAGS(IFRQ).EQ.0) FQTAGS(IFRQ)= FREQID
 100        CONTINUE
         ELSE

C           Enter un-matched group of frequencies in FQ table
            NFRQ = NFRQ + 1
            FREQID = NFRQ
            IFQRNO = NFRQ

C           Initialize entries in case there are fewer simultaneous
C           frequencies than the length of the IF axis.
            DO 105 J = 1, NUMIF
               IFFREQ(J) = 0.0
               IFCHW(J) = 0.0
               IFTBW(J) = 0.0
               IFSIDE(J) = 0
 105        CONTINUE

C           Set all (i.e., all IFs) table entries (offset, increment,
C           bandwidth, sideband) for this FREQID
            DO 110 J = 1, SELNIF(I)

C              Find RPFITS IF table entry number
               IFRQ = SELIF(I,J)

C              Where is this frequency going to go on the AIPS IF axis ?
               IFAXID = SELAX(IFRQ)

C              Fill wanted frequency
               IF (FQTAGS(IFRQ).EQ.0) THEN

C                 Channel separation
                  IF (NCHAN.LE.1) THEN
                     IFCHW(IFAXID) = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))
                  ELSE
                     IFCHW(IFAXID) = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))
     *                               /(NCHAN-1)
                  END IF
                  IF (IFCHW(IFAXID).EQ.0.0) THEN
                     MSGTXT = 'ATFQIN: Warning, zero frequency '
     *                        //'increment in FQ table'
                     CALL ATMSG (MSGTXT)
                  END IF

C                 Total bandwidth of selected channels
                  IFTBW(IFAXID) = ABS(IFCHW(IFAXID)) * NUMCHN
                  IF (HANN .AND. NCHNSL.EQ.1)
     *               IFTBW(IFAXID) = IFTBW(IFAXID) * CHANSL(3,1)

C                 Adjust reference pixel and increment for channel
C                 selection
                  IF (NCHNSL.EQ.1 .AND. CHANSL(3,1).NE.1) THEN
                     IFCHW(IFAXID) = IFCHW(IFAXID) * CHANSL(3,1)
                     REFPIX2 = (IF_REF(IFRQ) -
     *                 REAL(CHANSL(1,1)))/REAL(CHANSL(3,1)) + 1.0
                  ELSE
                     REFPIX2 = IF_REF(IFRQ) - BCHAN + 1
                  END IF

C                 Find the frequency of this scan at the reference
C                 pixel of the AIPS UV file; the RPFITS reference
C                 pixel may vary.
                  IF (REFPIX.NE.REFPIX2) THEN
                     FRQR = IF_FREQ(IFRQ) +
     *                      (REFPIX - REFPIX2)*IFCHW(IFAXID)
                  ELSE
                     FRQR = IF_FREQ(IFRQ)
                  END IF

C                 Difference between this and AIPS UV file
C                 reference frequency
                  IFFREQ(IFAXID) = FRQR - REFREQ

C                 Sideband indicator forced to 1 as increment signed
                  IFSIDE(IFAXID) = +1
               END IF
 110        CONTINUE

C           Enter it; watch out, IFQRNO gets incremented by TABFQ
            CALL TABFQ ('WRIT', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FREQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT, 120) IERR
 120           FORMAT ('ATFQIN: ERROR',I3,' WRITING FQ FILE.')
               CALL ATMSG (MSGTXT)
               GO TO 990
            END IF

C           Update FQTAGS for each IFNUM making up this FREQID
            DO 130 J = 1, SELNIF(I)

C              Find RPFITS IF table entry number
               IFRQ = SELIF(I,J)

C              Enter FREQID into translation table
               IF (FQTAGS(IFRQ).EQ.0) FQTAGS(IFRQ) = FREQID
 130        CONTINUE
         END IF

C        End loop over selected frequencies
 140  CONTINUE


C     If user wants to input XY Phases with the XYPHAS array, we need
C     to be able to convert from FREQID and IFAXID to the relevant
C     entry in the XYPHAS array.  It is assumed that the XYPHAS array
C     has been entered by the user in the SAME ORDER that frequencies
C     have been encountered in the data.  Thus, we basically just need
C     a counter for each selected and encountered frequency.
      IF (USERXY) THEN
C        Read through the FQ table
         K = 0
         DO 160 I = 1, NFRQ
            IFQRNO = I
            CALL TABFQ ('READ', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FREQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT, 60) IERR
               CALL ATMSG (MSGTXT)
               GO TO 990
            END IF

C           Loop over the entries on the IF axis and look out for unused
C           entries (not all simultaneous groups will necessarily have
C           the same length)
            DO 150 J = 1, NUMIF
               IF (IFSIDE(J).GT.0) THEN
                  K = K + 1
                  XYPHPT(J,I) = K
               END IF
 150        CONTINUE
 160     CONTINUE
      END IF

C     Close the FQ table.
 990  CALL TABIO ('CLOS', 0, 0, FQBUFF, FQBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATFQIN: ERROR CLOSING FQ (FREQUENCY) TABLE.'
         CALL ATMSG (MSGTXT)
      END IF


 999  RETURN
      END




      SUBROUTINE ATSUIN (IERR)
C-----------------------------------------------------------------------
C     ATSUIN sets the SUTAGS translation table from SU number in
C     the RPFITS IF_* tables to SOURCE ID in the AIPS SU table.
C
C     Given via common ATSEL:
C          FQTAGS(MAX_IF)
C                        I     Translation from frequency identification
C                              number in the RPFITS header to that of
C                              the FQ file.
C
C     Given via common ATCTRL:
C          OUTDSK        I     Output disk number.
C          NSUSEL        I     Number of sources WANTED
C          SUSEL         I     Pointers to selected sources
C                              in RPFITS SU table
C     Given via common ATUVIO:
C          CNO           I     UV file catalogue slot number.
C
C     Given via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Given via RPFITS common SU:
C          SU_FOUND      L     True if a source table was found for this
C                              scan.
C          NNSU          I     Number of entries in the RPFITS SU table.
C          SUNUM(MAX_SU)
C                        I     Source numbers corresponding to the
C                              SRCNUM random parameter for this scan.
C          SU_NAME(MAX_SU)
C                        C*16  Source names.
C
C    Given via common ATSEL:
C          NIFSEL        I     Number of groups of simultaneous
C                              frequencies found in users list
C          SELIF         I     List of pointers to frequencies in FQ
C                              table that user wants for each
C                              simultaneous pair of frequencies
C          SELNIF        I     Number of frequencies in each
C                              simultaneous group
C          SUTAGS(MAX_SU,MAX_IF)
C                        I     Translation from source identification
C                              number in the RPFITS header to that of
C                              the SU file.
C                                 -1: source is not wanted
C                                  0: source is wanted but not yet in
C                                     the SU table
C                                 >0: source is wanted and was found
C                                     in the SU table with this id.
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          ATLOD:  {ATMSG}
C          APLSUB: {MSGWRT, TABIO}
C          APLNOT: {SOUINI, TABSOU}
C
C     Algorithm:
C
C     Notes:
C       1)  The SU table is more difficult than the FQ table because
C         we need to distinguish between sources with the same name
C         but different frequencies.  This is so that the IQUV values
C         can be set differently for different frequencies in the SU
C         table.  There is no space in the SU table for FREQID, so we
C         put the QUAL = FREQID.   This is also why the SUTAGS array
C         is two dimensional, with the second axis being the FQTAGS.
C
C       2)  For simplicity, we build the SU and FQ tables at the
C         beginning of each scan.  Because we don't know exactly what
C         combination of sources and frequency may be in the data,
C         there may be some entries in the SU table that don't
C         correspond to any data. This can be dealt with by only
C         updating the SU table as visibilties are encountered (the
C         way it used to be), but at the cost of increased complexity.
C
C
C-----------------------------------------------------------------------
      DOUBLE PRECISION JUL2000
      PARAMETER (JUL2000 = 2451544.5D0)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'RPFITS.INC'

      INTEGER   SOUID, IERR, ISURNO, JERR,
     *           MAXIF4, NSRC, QUAL, SUBUFF(768),
     *          SUFREQ, SUKOLS(MAXSUC), SULUN, SUNUMV(MAXSUC),
     *          SUVER, IIF
      REAL      FLUX(4, MAXIF), POLAR(2)
      DOUBLE PRECISION  BANDW, DECAPP, DECEPO, EPOCH, FREQO(MAXIF),
     *          LRESTF(MAXIF), LSRVEL(MAXIF), PMDEC, PMRA, RAAPP,
     *          RAEPO, JULOBS, OBSPOS(3), EQUIN, RAOBS, DECOBS
      CHARACTER CALCOD*4, SOUNAM*16, VELDEF*8, VELTYP*8, OBSDAT*8

      PARAMETER (MAXIF4 = MAXIF*4)

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ATLOD.INC'

      INTEGER JLEN, JTRIM, I, J, K, IFRQ, ISRC, JSRC, NCHANS
      DATA OBSPOS, POLAR /3*0.0D0, 2*0.0/
C-----------------------------------------------------------------------
      IERR = 0

C     Deselect all sources.
      DO 20 ISRC = 1, MAX_SU
         DO 10 IFRQ = 1, MAX_IF
            SUTAGS(ISRC,IFRQ) = -1
 10      CONTINUE
 20   CONTINUE

C     Loop over desired sources
      DO 40 I = 1, NSUSEL
C
C        Find RPFITS SU table entry
         ISRC = SELSRC(I)

C        Loop over number of frequency groups
         DO 30 J = 1, NIFSEL

C           Loop over number of frequencies in this group
            DO 25 K = 1, SELNIF(J)

C              Find RPFITS IF table entry
               IFRQ = SELIF(J,K)

C              Make sure this frequency was not rejected
C              for incompatibility
               IF (FQTAGS(IFRQ).GT.0) THEN

C                 Mark this source at this frequency as wanted.
                  SUTAGS(ISRC,IFRQ) = 0
               END IF
 25         CONTINUE
 30      CONTINUE
 40   CONTINUE


C     Create or reopen the SU file.
      SUVER  = 1
      SULUN  = 41
      VELTYP = ' '
      VELDEF = ' '
      SUFREQ = -1
      CALL SOUINI ('WRIT', SUBUFF, OUTDSK, CNO, SUVER, CATBLK,
     * SULUN, NUMIF, VELTYP, VELDEF, SUFREQ, ISURNO, SUKOLS, SUNUMV,
     * IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATSUIN: ERROR OPENING THE SU (SOURCE ID) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


C     Loop through all selected sources
      ISURNO = 0
      NSRC = SUBUFF(5)
      DO 90 I = 1, NSUSEL

C        Find RPFITS table entry
         ISRC = SELSRC(I)

C        Loop through all selected groups of frequencies
         DO 80 J = 1, NIFSEL

C           Loop through all frequencies in this group
            DO 70 K = 1, SELNIF(J)

C              Find RPFITS table entry
               IFRQ = SELIF(J,K)

C              Do we want this combination ?
               IF (SUTAGS(ISRC,IFRQ).EQ.0) THEN


C                 Read through the SU table.
                  DO 60 JSRC = 1, NSRC
                     ISURNO = JSRC
                     CALL TABSOU('READ', SUBUFF, ISURNO, SUKOLS, SUNUMV,
     *                  SOUID, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *                  RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS,
     *                  DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
                     IF (IERR.GT.0) THEN
                        WRITE (MSGTXT, 50) IERR
 50                     FORMAT ('ATSUIN: ERROR',I3,' READING SU FILE.')
                        CALL ATMSG (MSGTXT)
                        GO TO 990
                     END IF

C                    Try to match source and frequency in SU table
                     IF (SU_NAME(ISRC).EQ.SOUNAM .AND.
     *                  FQTAGS(IFRQ).EQ.QUAL) THEN
                        SUTAGS(ISRC,IFRQ) = SOUID
                        GOTO 65
                     END IF
 60               CONTINUE
               END IF

C              Enter un-matched source/frequency into SU table
 65            IF (SUTAGS(ISRC,IFRQ).EQ.0) THEN
                  NSRC = NSRC + 1
C
                  ISURNO = NSRC
                  SOUID = NSRC
                  SOUNAM = SU_NAME(ISRC)
                  QUAL = FQTAGS(IFRQ)
                  CALCOD   = SU_CAL(ISRC)

C                 Replace NULLS
                  JLEN = JTRIM(CALCOD)

C                 IF dependent values
                  DO 66 IIF = 1, NUMIF

C                    Don't duplicate the FQ table offset here
                     FREQO(IIF) = 0.0D0

                     LSRVEL(IIF) = 0.0D0
                     LRESTF(IIF) = 0.0D0
                     FLUX(1,IIF) = 0.0
                     FLUX(2,IIF) = 0.0
                     FLUX(3,IIF) = 0.0
                     FLUX(4,IIF) = 0.0
 66               CONTINUE


                  NCHANS = IF_NFREQ(IFRQ) - 1
                  IF (NCHANS.EQ.0) NCHANS = 1
                  BANDW    = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ)/NCHANS)
                  RAEPO    = SU_RA(ISRC)*R2D
                  DECEPO   = SU_DEC(ISRC)*R2D
                  EPOCH    = 2000.0

C                 Precess mean coordinates to apparent. The on-line
C                 system  does not have the apparent coordinates at
C                 the beginning of the scan when it writes out the
C                 source table.  I don't know where the telescope
C                 is actually pointing at this time.

C                 RAAPP    = SU_RAD(ISRC)*R2D
C                 DECAPP   = SU_DECD(ISRC)*R2D

C                 Julian date at the start of the observation.
                  CALL H2CHR (8, 1, CATR(KHDOB), OBSDAT)
                  CALL JULDAY (OBSDAT, JULOBS)

C                 Precess
C                 CALL PRECES (JULOBS, 2000.D0, 0.1D0, SU_RA(ISRC),
C    *                         SU_DEC(ISRC), RAAPP, DECAPP, .TRUE.,
C    *                         .FALSE., .FALSE., 0.0D0, 0.0D0,
C    *                         0.0D0, .FALSE.)
                  EQUIN = 2000.0D0
                  CALL JPRECS (JULOBS, EQUIN, 1.D-6, 1, .TRUE.,
     *               OBSPOS, POLAR, SU_RA(ISRC), SU_DEC(ISRC), RAAPP,
     *               DECAPP)

                  RAAPP    = RAAPP*R2D
                  DECAPP   = DECAPP*R2D

                  PMRA     = 0D0
                  PMDEC    = 0D0
C                                       not guaranteed to be right
                  RAOBS = RAEPO
                  DECOBS = DECEPO

C                 Watch out, ISURNO gets incremented by TABSOU
                  CALL TABSOU('WRIT', SUBUFF, ISURNO, SUKOLS, SUNUMV,
     *              SOUID, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *               RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *               LSRVEL, LRESTF, PMRA, PMDEC, IERR)
                  IF (IERR.GT.0) THEN
                     WRITE (MSGTXT, 67) IERR
 67                  FORMAT ('ATSUIN: ERROR',I3,' WRITING SU FILE.')
                     CALL ATMSG (MSGTXT)
                     GO TO 990
                  END IF

C                 Update SUTAGS.
                  SUTAGS(ISRC,IFRQ) = SOUID

               END IF
 70         CONTINUE
 80      CONTINUE
 90   CONTINUE

C     Close the SU table.
 990  CALL TABIO ('CLOS', 0, 0, SUBUFF, SUBUFF, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'ATSUIN: ERROR CLOSING SU (SOURCE ID) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


 999  RETURN
      END


      SUBROUTINE ATANT (IERR)
C-----------------------------------------------------------------------
C     ATANT builds the ANtenna file from the ANTENNA cards found in the
C     RPFITS header.
C
C     Given via parameters in ATLOD.INC:
C          NAC           I     Number of Compact Array antennas.
C
C     Given via common ATCTRL:
C          OUTDSK        I     Output disk number.
C
C     Given via common ATUVIO:
C          CNO           I     UV file catalogue slot number.
C
C     Given via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Given via RPFITS common ANTEN:
C          NANT          I     The number of antennas.
C          ANT_NUM()     I     Antenna number.
C          STA()         C*8   Station name.
C          X(),Y(),Z()   D     Antenna coordinates, in metres.
C
C     Given via RPFITS common EPHEM:
C          RP_UTCMTAI    D     UTC-TAI, in seconds.
C
C     Returned via common ATSCAN:
C          ANTIN()       L     True if antenna in array. e.g., if
C                              ANTIN(3) = .FALSE. then antenna 3 is
C                              not in the array.
C
C     Returned via common VISBUF:
C          BASLEN(,)     R     Compact array baselines, in meters.
C          BASMIN        R     Minimum baseline length, in meters.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          APLSUB: {ANTINI, AXEFND, COPY, H2CHR, ISTAB, JULDAY, MSGWRT,
C                   RFILL, TABAN, TABIO}
C          APLNOT: {GSTROT}
C
C     Algorithm:
C
C     Notes:
C       1) At this time there are many variables to be stored in the
C          ANtenna file which are not provided in the RPFITS header.
C          These are left to default.
C
C       2) Many AIPS tasks do not distinguish between the row number in
C          the ANtenna table and the antenna number itself.  Dummy
C          antenna entries are added wherever needed to ensure this is
C          the case.
C
C       3) Tolerance for antenna locations is 1 cm, but really,
C          7.5 m would do !
C
C       4) Support for LBA polzn and "XY" antenna mounts added by
C          Richard Dodson
C
C-----------------------------------------------------------------------

      INCLUDE 'INCS:PUVD.INC'

      LOGICAL   EXIST, FITASC, TABLE, ZEROED, OVERWR
      INTEGER   ANLUN, ANBUFF(512), IA1, IA2, IANT, IERR, IOFF, JANT,
     *          JERR, NUMANT, IVER
      DOUBLE PRECISION GASTM, JD, LOCTOL
C      CHARACTER*8 TIMSYS

      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'

      PARAMETER (LOCTOL = 1.0D-2)
      DATA ANLUN /28/
C-----------------------------------------------------------------------
C  Does the antenna file exist already?
      CALL ISTAB ('AN', OUTDSK, CNO, 1, ANLUN, ANBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 10) IERR
 10      FORMAT ('ATANT: ERROR',I3,' FINDING ANTENNA TABLE.')
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

      IF (.NOT.EXIST) THEN
C        Setup for AN table initialization.
         NUMORB = 0
         NOPCAL = 2
         ANTNIF = NUMIF
         ANFQID = -1

C        Position of the earth's pole.
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = 0.0
C         IATUTC = -RP_UTCMTAI
         DATUTC = 0.0

C        Array name.
         CALL H2CHR (8, 1, CATH(KHTEL), ANAME)

C        Array center relative to the geocentre (zero by definition).
         ARRAYC(1) = 0D0
         ARRAYC(2) = 0D0
         ARRAYC(3) = 0D0

C        Get GST at IAT=0, and Earth rotation rate.
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)

C        Get frequency.
         IOFF = 0
         CALL AXEFND (4, 'FREQ', KICTPN, CATH(KHCTP), IOFF, IERR)
         SAFREQ = CATD(KDCRV+IOFF)

      END IF

C     Create/init file
      IVER = 1
      TIMSYS = 'UTC     '
      CALL ANTINI ('WRIT', ANBUFF, OUTDSK, CNO, IVER, CATBLK, ANLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *   NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 20) IERR
 20      FORMAT ('ATANT: ERROR',I3,' OPENING ANTENNA TABLE.')
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


C  Enter each antenna in the AN table if it's not already there.
      NUMANT = ANBUFF(5)
      DO 60 IANT = 1, NANT

C        Check for dummy entries.
         ZEROED = X(IANT).EQ.0D0 .AND.
     *            Y(IANT).EQ.0D0 .AND.
     *            Z(IANT).EQ.0D0

*>                                        JReynolds 2001/10/10
C  Catch illegal ANT_NUM values (e.g. LBA data pre-30/07/1996) and
C  do the best you can. It may be better at some point to ignore
C  ANT_NUM entirely and simply use order (IANT).
         IF (ANT_NUM(IANT).LE.0) THEN
            WRITE (MSGTXT, 25) STA(IANT),ANT_NUM(IANT),IANT
 25         FORMAT ('WARNING! ANTENNA ',A,' ANT_NUM',I4,' =>',I2,'.')
            CALL ATMSG (MSGTXT)
            ANT_NUM(IANT) = IANT

         ELSE IF (ANT_NUM(IANT).GT.ANT_MAX) THEN
            WRITE (MSGTXT, 25) STA(IANT),ANT_NUM(IANT),ANT_MAX
            CALL ATMSG (MSGTXT)
            ANT_NUM(IANT) = ANT_MAX
         END IF
*<
*>                                        JReynolds 2000/01/26
         ANTIN(ANT_NUM(IANT)) = .NOT.(ZEROED .AND. INSTRUMENT.EQ.'ATCA')
*=
*        ANTIN(ANT_NUM(IANT)) = .NOT.ZEROED
*<

C        Search for this antenna in the AIPS AN table.
         IANRNO = 1
         OVERWR = .FALSE.
         MNTSTA = -1
         DIAMAN = 0.0
         CALL RFILL (MAXANT, 0.0, FWHMAN)
         DO 40 JANT = 1, NUMANT
            CALL TABAN ('READ', ANBUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 30) IERR
 30            FORMAT ('ATANT: ERROR',I3,' READING THE ANTENNA TABLE.')
               CALL ATMSG (MSGTXT)
               GO TO 990
            END IF

            IF (NOSTA.EQ.ANT_NUM(IANT)) THEN
C              Found the antenna, check the coordinates.
               IF (ABS(STAXYZ(1)-X(IANT)).LE.LOCTOL .AND.
     *             ABS(STAXYZ(2)-Y(IANT)).LE.LOCTOL .AND.
     *             ABS(STAXYZ(3)-Z(IANT)).LE.LOCTOL) THEN
                  GO TO 60
               ELSE IF (ZEROED) THEN
C                 If this antenna is already in the AN table, then, as
C                 it has zeroed coordinates in the current RPFITS
C                 antenna table, leave the current AN table entry as is

                  GO TO 60
               ELSE
                  IF (STAXYZ(1).EQ.1.0D0 .AND. STAXYZ(2).EQ.1.0D0 .AND.
     *                STAXYZ(3).EQ.1.0D0) THEN

C                    This antenna is zeroed (oned) in the current AN
C                    table. Possibly, the antenna was out of the array
C                    and has now been picked up.  Replace this entry
C                    in the AN table with the current one.

                     OVERWR = .TRUE.
                     GO TO 45
                  ELSE

C                    Possibly a different array configuration.

                     MSGTXT = 'ATANT: INCONSISTENT ANTENNA ' //
     *                        'COORDINATES, ABORT!'
                     CALL ATMSG (MSGTXT)
                     MSGTXT = '(ATLOD will not concatenate ' //
     *                        'different array configurations)'
                     CALL ATMSG (MSGTXT)
                     IERR = 1
                     GO TO 990
                  END IF
               END IF
            ELSE IF (.NOT.ZEROED) THEN
C              Compute the baseline length for shadowing calculations.
               IA1 = NOSTA
               IA2 = ANT_NUM(IANT)
               IF (BASLEN(IA1,IA2).EQ.1E20) THEN
                  IF (STAXYZ(1).NE.1D0 .AND.
     *                STAXYZ(2).NE.1D0 .AND.
     *                STAXYZ(3).NE.1D0) THEN
                     BASLEN(IA1,IA2) = SQRT((X(IANT)-STAXYZ(1))**2 +
     *                                      (Y(IANT)-STAXYZ(2))**2 +
     *                                      (Z(IANT)-STAXYZ(3))**2)
                     BASLEN(IA2,IA1) = BASLEN(IA1,IA2)
                     BASMIN = MIN(BASMIN, BASLEN(IA1,IA2))
                  END IF
               END IF
            END IF
 40      CONTINUE

C        Initialize the basic AN record.
 45      STAXOF = 0.0
         ORBPRM(1) = 0D0
         NOSTA = 0
         if (aname.eq.'ATLBA') then
C          XY mount support -
C          ANT_MOUNT is an RPFITS array accessed via common block
           MNTSTA = ANT_MOUNT(IANT)
C          LBA observations are always circular pol.
           POLAA = 0.0
           POLAB = 0.0
           POLTYA = 'R'
           POLTYB = 'L'
         else
           MNTSTA = 0
           POLAA = 45.0
           POLAB = 135.0
           POLTYA = 'X'
           POLTYB = 'Y'
         endif
         CALL RFILL (NOPCAL, 0.0, POLCA)
         CALL RFILL (NOPCAL, 0.0, POLCB)

C        Fill in the specifics
         NOSTA  = ANT_NUM(IANT)
         ANNAME = STA(IANT)
         STAXYZ(1) = X(IANT)
         STAXYZ(2) = Y(IANT)
         STAXYZ(3) = Z(IANT)

C        This is for the benefit of bad AIPS code.
         IF (ZEROED) THEN
            STAXYZ(1) = 1D0
            STAXYZ(2) = 1D0
            STAXYZ(3) = 1D0
            END IF
C        Write the AIPS AN record.  Overwrite existing record if an
C        antenna has come into the array part way through.
         IF (OVERWR) THEN
            IANRNO = IANRNO - 1
         ELSE
            IANRNO = NUMANT + 1
            END IF
         DIAMAN = 0.0
         CALL RFILL (MAXANT, 0.0, FWHMAN)
         CALL TABAN ('WRIT', ANBUFF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 50) IERR
 50         FORMAT ('ATANT: ERROR',I3,' WRITING THE ANTENNA TABLE.')
            CALL ATMSG (MSGTXT)
            GO TO 990
         END IF

         IF (.NOT.OVERWR) NUMANT = NUMANT + 1
 60   CONTINUE


C  Close the ANtenna file.
 990  CALL TABIO ('CLOS', 0, 0, ANBUFF, ANBUFF, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'ATANT: ERROR CLOSING AN (ANTENNA) TABLE.'
         CALL ATMSG (MSGTXT)
      END IF

 999  RETURN
      END



      SUBROUTINE ATUV (TTY, DOTCLS, NSUVIS, MAXREJ,
     *                 NREJCT, NZFILL, NSSFIX, SSSUM, SSMIN, SSMAX,
     *                 NPHCLP, NTXCLP, NTYCLP, IERR)
C-----------------------------------------------------------------------
C     ATUV retrieves data from an RPFITS file and writes it into an
C     AIPS uv data file.
C
C     Parameters set by ATLOD.INC:
C          NSYSCP        I     Number of syscal parameters in the
C                              SYSNUM array.
C
C     Given via common ATCTRL:
C          BCHAN         I     First channel to load
C          OUTDSK        I     Output disk number.
C          TIMRNG(2)     R     Time range selected.
C          DOUVCM        L     If true, use compressed uv data format.
C          DOSTOK        L     If true convert linear polarizations to
C                              Stokes.
C          DROFLG        L     If true drop flagged visibilities .
C          DOAUTO        L     If true, retain autocorrelations.
C          SHADOW        R     Shadowing diameter, in meters.
C          NXGAP         R     Maximum gap before starting a new scan
C                              index (NX) entry, in days.
C          NXSPAN        R     Maximum length of a scan index (NX)
C                              entry, in days.
C          EXBSLN(NCA,NCA)
C                        L     Baselines to be dropped.
C          DONPHA        L     If true, negate visibility phases.
C          DOXYFL        L     If true drop visibilties when XY
C                              phases bad.
C          USERXY        L     If true, use user guven XYPHASE array
C          YGNROT        L     If true, apply XY phase to Y gain
C          TRUPOL        I     Describes raw polarization type before
C                              conversion or fudging
C                                 1: XX
C                                 2: YY
C                                 3: XX YY
C                                 4: XX YY XY YX
C                                 0: something else
C     Given via common ATSCAN:
C          ANTIN()       L     True if antenna in array. e.g., if
C                              ANTIN(3) = .FALSE. then antenna 3 is
C                              not in the array.
C
C     Given via commons ATUVIO and ATUVCH:
C          UVNAME        C*48  AIPS UV file name.
C          UVLUN         I     UV file logical unit number.
C          UVFIND        I     FTAB UV file pointer returned by ZOPEN.
C          LREC          I     Number of values in a visibility record.
C          TBSORT        L     True if the data is in time sequence.
C          REFREQ        D     Reference frequency, (Hz).
C
C     Given via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Given via RPFITS common IF:
C          IF_INVERT(MAX_IF)
C                        I     Is -1 if the video is inverted, +1
C                              otherwise.
C
C     Given via RPFITS common SC:
C          SC_Q          I     Number of syscal parameters per IF per
C                              antenna.
C          SC_IF         I     Number of IFs per antenna.
C          SC_ANT        I     Number of antennas.
C          SC_CAL(SC_Q,SC_IF,SC_ANT)
C                        R     Syscal parameters.
C
C     Given
C          TTY(2)        I     Terminal I/O buffer
C     Given and returned:
C          DOTCLS        L     True if terminnal yet to be closed
C                              when listing SYSCAL info
C
C     Given and returned via common ATSEL:
C          FQTAGS(MAX_IF)
C                        I     Translation from frequency identification
C                              number in the RPFITS header to that of
C                              the FQ table.
C          SUTAGS(MAX_SU,MAX_IF)
C                        I     Translation from source identification
C                              number in the RPFITS header to that of
C                              the SU table.
C
C     Given and returned via common ATCTRL:
C          NBLKS         I     Number of blocks in UV file.
C
C     Given and returned via common ATUVIO:
C          NPIO          I     Maximum uv records per transfer.
C          UVBUFF(*)     R     UV IO buffer.
C          UVBIND        I     Pointer into UVBUFF.
C          NIO           I     Number of visibilities to be written.
C          VISCNT        I     Total number of visibilities in the UV
C
C     Given and returned via common ATSCAN:
C          PREVFQ        I     FQ id of the previous scan.
C          PREVSU        I     SU id of the previous scan.
C          TSCAN2        D     Scan end time (days).
C          VSCAN2        I     Index of the last visibility.
C
C     Given and returned via common ATSYSC:
C          PHSAVE(NCA,MAXSFQ)
C                        R     Average of XY phase stacks.  Phases are
C                              accumulaed for each antenna frequency,
C                              and IF.
C          TXAVE(NCA,MAXSFQ,MAXSSU)
C                        R     Average of X Tsys stacks.
C          TYAVE(NCA,MAXSFQ,MAXSSU)
C                        R     Average of Y Tsys stacks.
C                              System temperatures are accumulated for
C                              each antenna, frequency, IF, and source.
C
C     Returned:
C          NSUVIS(MAX_SU,MAX_IF,2)
C                        I     Number of visibilities accepted and
C                              rejected for each source.
C          NREJCT(MAXREJ,MAX_IF)
C                        I     Number of visibilities rejected for
C                              each IF.  This is the RPFITS IF number,
C                              not the AIPS IF axis number.  They
C                              might be different.
C                                 1: invalid random parameters
C                                 2: system calibration records
C                                 3: flagged by the correlator
C                                 4: frequency selection
C                                 5: source selection
C                                 6: time range selection
C                                 7: autocorrelations
C                                 8: rejected baselines
C                                 9: baseline shadowing
C                                10: Stokes conversion or Tsys
C                                    correction impossible because
C                                    syscal info not received
C                                11: XY phase clipped so all
C                                    visibilities involving clipped
C                                    antenna dropped
C                                12: Sampler statistics bad
C                                13: Antenna not in array according
C                                    to CAOBS, but still in data.
C
C          NZFILL(MAX_IF)I     Number of visibilties zero filled
C                              for each IF axis location
C          NSSFIX(MAX_IF)I     Number of visibilities that were
C                              corrected for bad sampler statistics
C          SSSUM(MAX_IF) R     Sum of average (XX,YY,XY,YX) sampler
C                              correction factors for this scan
C          SSMIN,MAX(MAX_IF)
C                        R     Min and max correction factors
C          NPH,TX,TYCLP  I
C                              NUmber points clipped for bad XY phases
C                              Tx and Ty per antenna per IF
C          IERR          I     Error status
C                                -1  end-of-file
C                                 0  encountered a header, FG table, end
C                                    of scan, or illegal data.  Next
C                                    action is to (re)read a header
C                                 1  fatal error
C
C     Returned via common ATSCAN:
C          TSCAN1        D     Scan start time (days).
C          VSCAN1        I     Index of the first visibility.
C          SCANTN(NCA)   L     True for each antenna used in the scan.
C
C     Called:
C          ATLOD:  {ATNXCL, ATMSG, CLPWRN, FIXSRT, LINSTK, LINROT,
C                   SHADIN, SYSCIN, TSYSCO, UVACUM, UVDUMP}
C          RPFITS: {RPFITSIN}
C          APLSUB: {H2CHR, JULDAY, MSGWRT, RELPOP}
C          APLGEN: {ZEXPND}
C
C     Algorithm:
C
C     Notes:
C       1) ATUV does most of the work for ATLOD.  Anything inside the
C          main loop should be written efficiently.
C
C       2) The UT returned by RPFITSIN is assumed to range past 24 hours
C          so that there is no ambiguity in the day number.
C
C       3) (u,v,w) for autocorrelation visibilities is forced to be
C          (0,0,0).  It was originally recorded as (1,1,1).
C
C       4) System calibration records should be found before the
C          correlations begin for each integration.  When making
C          corrections for bad samplers, it is ASSUMED that the
C          sampler values in the SYSNUM array come from the
C          current intregration.  If a syscal group did not appear
C          you would get whatever was left over from the previous
C          syscal group.   This might occurr once in a blue moon but
C          is not worth the effort to fix up.
C
C       5) If conversion to Stokes parameters is requested, the system
C          XYphase differences are edited to remove badly discrepant
C          points.  A running mean is maintained and bad points are
C          replaced by the mean.  Accumulation continues across scan
C          boundaries, source and frequency changes.  The actual
C          visibilities associated with all baselines derived from
C          the bad antenna are not loaded.  This editing procedure can
C          be invoked even when not converting to Stokes.
C
C       6) If Tsys correction requested, then the Tsys values are
C          edited in the same way as 5) above.  Any ON-line Tsys
C          correction will be undone and the OFF-line averaged Tsys
C          correction done over DOTSYS integrations
C
C       7) When simultaneous frequencies are recorded, the order that
C          the data are read is  SYSCAL_FQ1, VIS_FQ1, SYSCAL_FQ2,
C          VIS_FQ2 where VIS represents all possible baselines.  This
C          requires that the data be accumulated for one integration
C          before being reordered to use the IF axis for simultaneous
C          data, and then being written out.
C
C
C-----------------------------------------------------------------------

      INCLUDE 'RPFITS.INC'

      LOGICAL   NEWNDX, FIRST, DOTCLS
      INTEGER   BPOINT, DHMS(4), FREQID, I, J,
     *   IA1, IA2, IANT, SOUID, IERR, IFRQ, ISRC, JERR, JSTAT,
     *   NBYPR, NREC, MAXREJ, NREJCT(MAXREJ,MAX_IF), IFAXID,
     *   NSUVIS(MAX_SU,MAX_IF,2), NUMFRQ, NVIS, NXPND, IOBLK(256),
     *   TTY(2), NUMPOL, NBASE,  NZFILL(MAX_IF)
      INTEGER ITRIM
      REAL      SGN, FACXX, FACYY, FACXY, FACYX,
     *   OLDUT, SSMIN(MAX_IF), SSMAX(MAX_IF), SSSUM(MAX_IF)

      DOUBLE PRECISION  JDAY, JDAY0, TSCAN, TIME, XBLKS, XBYPR, XBPS
      CHARACTER CHTM8*8

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'ATLOD.INC'

C     System calibration information variables.  The *_MAX_* parameters
C     are from RPFITS.INC
      DOUBLE PRECISION SCTIME, OLDSCT
      REAL      SYSNUM(ANT_MAX,MAX_IF,NSYSCP), SYSSGN, SSFAC,
     *          SSMN, SSMX
      INTEGER NSSFIX(MAX_IF), NPHCLP(ANT_MAX,MAX_IF),
     *   NTXCLP(ANT_MAX,MAX_IF), NTYCLP(ANT_MAX,MAX_IF)
      INTEGER   STKA1, STKA2, LOCDOT, AXEUSE,BAD1, FLAG_IF1
      LOGICAL   OFFLIS, BADSS(ANT_MAX,MAX_IF), SSFIX, SSHEAD,
     *          SSTDUN(ANT_MAX,MAX_IF)

C-----------------------------------------------------------------------
C     Extract axis dimensions from header of output file
      NUMPOL = CATBLK(KINAX+1)

C     The user may have specified one group of channel, and given an
C     increment other than 1.  The unselected channels are physically
C     dropped in this case.   Find the number of channels before the
C     unselected ones are dropped.  This is because channel dropping
C     is the last thing done, during the copy to the output buffer
      NUMFRQ = ECHAN - BCHAN + 1

C  Initialize.
C     Visibility acceptance and rejection counters
      IERR = 0
      NVIS = 0
      DO 20 IFRQ = 1, MAX_IF

C        Number of vis zero filled in specified IF axis locations
         NZFILL(IFRQ) = 0

C        Number vis corrected  for bad samplers per IFNUM
         NSSFIX(IFRQ) = 0

C        Sum of average (XX,YY,XY,YX)sampler correction per IFNUM
         SSSUM(IFRQ) = 0.0

C        Extreme sampler corrections
         SSMAX(IFRQ) = -1.0
         SSMIN(IFRQ) = 100.0
C
         DO 10 ISRC = 1, MAX_SU

C           Number of vis accepted per source and IFNUM
            NSUVIS(ISRC,IFRQ,1) = 0

C           Number of vis rejected per source and IFNUM
            NSUVIS(ISRC,IFRQ,2) = 0
 10      CONTINUE
 20   CONTINUE

C     Counters for different types of rejected visbilities
      DO 30 I = 1, MAXREJ
         DO 25 J = 1, MAX_IF
            NREJCT(I,J) = 0
 25      CONTINUE
 30   CONTINUE

C     Clipped SYSCAL records
      DO 40 J = 1, MAX_IF
         DO 35 I = 1, NANT
            NPHCLP(I,J) = 0
            NTXCLP(I,J) = 0
            NTYCLP(I,J) = 0
 35      CONTINUE
 40   CONTINUE

C     Baseline masks
      DO 45 I = 1, 256*NCA+NCA
         BASIND(I) = 0
 45   CONTINUE
C
      DO 55 I = 1, NCA*(NCA-1)/2+NCA
         INDBAS(I) = 0
         RANDUN(I) = .FALSE.
         DO 50 J = 1, NUMIF
            BASAX(I,J) = 0
 50      CONTINUE
 55   CONTINUE
      IBASE = 0
      NBASE = 0

C     Julian date at the start of the observation.
      CALL H2CHR (8, 1, CATR(KHDOB), CHTM8)
      CALL JULDAY (CHTM8, JDAY0)

C     Julian date from the present scan header.
      CALL JULDAY (DATOB8, JDAY)

C     Local SCAN based UT offset
      OLDUT = 0.0
      UT = 0.0
      SCTIME = 0.0
      OLDSCT = 0.0

C     Have we written a bad sampler time header yet ?
      SSHEAD = .FALSE.

C     Scan based Tsys correction control
      LOCDOT = DOTSYS
      IF (LOCDOT.GT.1) THEN
         IF (JDAY.LT.JULSYS) THEN
            MSGTXT = 'WARNING! Data too old for Tsys correction.'
            CALL ATMSG (MSGTXT)
            LOCDOT = 0
         ELSE IF (JDAY.LT.JULSRC) THEN
            IF (NNSU.GT.1) THEN
               MSGTXT = 'WARNING! Found multiple sources/scan'
     *                     //' but these data are too old'
               CALL ATMSG (MSGTXT)
               MSGTXT = 'WARNING! for SYSCAL source selection;'
     *                     //' Tsys correction disabled'
               CALL ATMSG (MSGTXT)
               LOCDOT = 0
            END IF
         ELSE IF (TRUPOL.LT.1 .OR. TRUPOL.GT.4) THEN
            MSGTXT = 'WARNING! Polarizations not linear, can''t ' //
     *               'correct for Tsys.'
            CALL ATMSG (MSGTXT)
            LOCDOT = 0
         END IF
      END IF

C     Start location in complex VIS buffer for first wanted channel
      BPOINT = (NUMPOL*(BCHAN-1)) + 1


      SYSSGN = 1.0
      IF (DOSNEG) SYSSGN = -SYSSGN
      FIRST = .TRUE.


C     Loop until we exhaust data in this scan.
      BAD1 = 0
 60   CONTINUE
C        Get the next visibility
         JSTAT = 0
         CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W, FLAG,
     *      BIN, IFNUM, SRCNUM)
*>                                                 JER 1997/08/27
C     If required, copy flagging from IFNUM=1 to higher IFs
C     (for PTI data pre-1997).
         if (ifnum.eq.1) then
            flag_if1=flag
         else if (flgfdg .and. ifnum.gt.1) then
            flag=flag_if1
         end if
*<
C        Fudge for March 1992 single source per scan data with
C        scrambled source numbers following moasicing.
         IF (XDPARM(1).GT.0.0) SRCNUM = 1

         IERR = 0
         IF (JSTAT .NE. 5) THEN
            BAD1 = 0
         END IF
         IF (JSTAT.NE.0) THEN
            IF (JSTAT.EQ.-1) THEN
               TIME = (JDAY - JDAY0) + UT/86400D0
               CALL TIMCON (TIME, DHMS)
               WRITE (MSGTXT, 62) DHMS
 62            FORMAT ('ATUV: UNABLE TO READ DATA, LAST UT = ',I3, I3.2,
     *                 ':', I2.2,':', I2.2, ' SKIP TO NEXT SCAN')
               CALL ATMSG  (MSGTXT)
C
               NSEVER = MIN(NSEVER+1, MAXSEV)
               SEVERE(NSEVER) = MSGTXT
            ELSE IF (JSTAT.EQ.1) THEN
C              Note that the next attempt to read a header will read the
C              one just encountered here, not the one beyond.
            ELSE IF (JSTAT.EQ.2) THEN
               MSGTXT = 'Normal end of scan encountered.'
               CALL ATMSG (MSGTXT)
            ELSE IF (JSTAT.EQ.3) THEN
C              End-of-file encountered.
               IERR = -1
            ELSE IF (JSTAT.EQ.4) THEN
               MSGTXT = 'Flagging table encountered and ignored.'
               CALL ATMSG (MSGTXT)
            ELSE IF (JSTAT.EQ.5) THEN
               IF (BAD1 .EQ. 0) THEN
C                 1st occurrence of jstat=5 should be ignored and next
C                 record read.
                  BAD1 = 1
                  GO TO 60
               END IF
               TIME = (JDAY - JDAY0) + UT/86400D0
               CALL TIMCON (TIME, DHMS)
               WRITE (MSGTXT, 65) DHMS
 65            FORMAT ('ATUV: INVALID DATA, LAST UT = ', I3, I3.2, ':',
     *                 I2.2,':', I2.2, ' SKIP TO NEXT SCAN')
               CALL ATMSG  (MSGTXT)
C
               NSEVER = MIN(NSEVER+1, MAXSEV)
               SEVERE(NSEVER) = MSGTXT
            END IF

C           Write out any data sitting in the accumulation buffer
            CALL UVDUMP (NBASE, NZFILL, OLDUT, JERR)
            IF (JERR.NE.0) THEN
               IERR = 1
               GOTO 999
            END IF

C           Update the NX and CL tables.
            IF (TBSORT .AND. NVIS.GT.0) THEN
               CALL ATNXCL (NANT, FREQID, SOUID, JERR)
               IF (JERR.NE.0) THEN
                  MSGTXT = 'ATUV: ERROR UPDATING NX AND CL TABLES.'
                  CALL ATMSG (MSGTXT)
                  CALL FIXSRT (JERR)
               END IF
            END IF

C           Report on the quality of the system calibration records
            CALL CLPWRN (ANT_MAX, MAX_IF, IF_FREQ, DOXYFL, DOSTOK,
     *                   LOCDOT, NANT, ANTIN, MAXREJ, NREJCT, NPHCLP,
     *                   NTXCLP, NTYCLP)
            GO TO 999
         END IF

C        Announce the scan start time.
         IF (FIRST) THEN
            TSCAN = (JDAY - JDAY0) + UT/86400D0
            IF (TSCAN.NE.0.0) THEN
               CALL TIMCON (TSCAN, DHMS)
               WRITE (MSGTXT, 70) DATOBS, DHMS
 70            FORMAT ('Date: ',A,'  UT start:',I3,I3.2,':',I2.2,':',
     *            I2.2)
               CALL ATMSG (MSGTXT)
               FIRST = .FALSE.
            END IF
         END IF

C        Set source number of SYSCAL groups
         IF (BASELN.EQ.-1) THEN
            IF (JDAY0.GE.JULSRC) THEN
               SRCNUM = SC_SRCNO
            ELSE
C              Old data only had one source per scan and no SC_SRCNO
               SRCNUM = 1
            END IF
         END IF

C        Check legitimacy of SRCNUM.
         IF (SRCNUM.LT.1 .OR. SRCNUM.GT.MAX_SU) THEN
            NREJCT(1,1) = NREJCT(1,1) + 1
            GO TO 60
         END IF

C        Process system calibration records.
         IF (BASELN.EQ.-1) THEN
            SCTIME = (JDAY - JDAY0) + SC_UT/86400D0
            CALL TIMCON (SCTIME, DHMS)

C           Does this syscal group come from a new integration ?
            IF (ABS(SCTIME-OLDSCT).GT.1D0/86400D0) THEN
               SSHEAD = .FALSE.

C              Initialize logicals that indicate if bad sampler
C              stats have been reported for a given antenna
C              each integration
               DO 80 I = 1, MAX_IF
                  DO 75 J = 1, ANT_MAX
                     SSTDUN(J,I) = .FALSE.
 75               CONTINUE
 80            CONTINUE
            END IF

            IF (PRTSYS.EQ.1 .OR. PRTSYS.EQ.3) THEN
C              List calibration information to terminal.
               CALL SYSLIS (R2D, SC_Q, SC_IF, SC_ANT, SC_CAL, DHMS,
     *                      ANTIN, TTY, CHKPNT, OFFLIS, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT, 90) IERR
 90               FORMAT ('I/O error with terminal. IERR = ', I3)
                  GO TO 999
               END IF

C              Quit printout, close terminal and restart AIPS.
               IF (OFFLIS) THEN
                  DOTCLS = .FALSE.
                  CALL ZCLOSE (TTY(1), TTY(2), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT, 100) IERR
 100                 FORMAT ('Error closing terminal.  IERR = ', I3)
                     IERR = 0
                  END IF
C
                  PRTSYS = 0
                  IF (RQUICK) CALL RELPOP (0, IOBLK, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT, 110) IERR
 110                 FORMAT ('Error resuming AIPS, IERR = ', I3)
                     CALL ATMSG (MSGTXT)
                     IERR = 0
                  END IF
               END IF
            END IF

C           Process SYSCAL info if required
            IF ( ((DOSTOK .OR. YGNROT) .AND. .NOT. USERXY) .OR.
     *             DOXYFL .OR. (LOCDOT.EQ.-1 .OR. LOCDOT.GT.1) .OR.
     *             SSCHK.GT.0) THEN

C              Process SYSCAL group, applying editing, averaging
C              and checking
               CALL SYSCIN (LOCDOT, DHMS, SYSNUM,
     *                      BADSS, NPHCLP, NTXCLP, NTYCLP, MAXREJ,
     *                      NREJCT, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'ATUV: SYSCAL ERROR'
                  CALL ATMSG (MSGTXT)
                  GOTO 999
               END IF
            END IF

C           Update last syscal time
            OLDSCT = SCTIME

C           Read the next visibility.
            GO TO 60
         ELSE IF (BASELN.LE.256) THEN
C           Something is wrong with this visibility.
            WRITE (MSGTXT, 120)
 120        FORMAT ('WARNING! Unrecognizable baseline in scan, your',
     *              'data may be corrupt.')
            CALL ATMSG (MSGTXT)

C           Read next visibility
            NREJCT(1,1) = NREJCT(1,1) + 1
            GOTO 60
         END IF

C        Check legitimacy of IFNUM.
         IF (IFNUM.LT.1 .OR. IFNUM.GT.MAX_IF) THEN
            NREJCT(1,1) = NREJCT(1,1) + 1
            GO TO 60
         END IF

C        Check for flagged data.
         IF (DROFLG .AND. FLAG.NE.0 .AND. BASELN.NE.-1) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(3,IFNUM) = NREJCT(3,IFNUM) + 1
            GO TO 60
         END IF

C        Get the FQ id. and AIPS IF axis location
         IF (FQTAGS(IFNUM).EQ.-1) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(4,1) = NREJCT(4,1) + 1
            GO TO 60
         ELSE
            FREQID = FQTAGS(IFNUM)
            IFAXID = SELAX(IFNUM)
         END IF

C        Get the SU id.
         IF (SUTAGS(SRCNUM,IFNUM).EQ.-1) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(5,IFNUM) = NREJCT(5,IFNUM) + 1
            GO TO 60
         ELSE
            SOUID = SUTAGS(SRCNUM,IFNUM)
         END IF

C        Convert from UT to days and check the time range.
         TIME = (JDAY - JDAY0) + UT/86400D0
         IF (TIME.LT.DBLE(TIMRNG(1)) .OR. TIME.GT.DBLE(TIMRNG(2))) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(6,IFNUM) = NREJCT(6,IFNUM) + 1
            GO TO 60
         END IF

C        Check required baselines.
         IA1 = BASELN/256
         IA2 = BASELN - 256*IA1

C        Antennas not in array according to CAOBS, but correlator
C        doesn't know
         IF (.NOT.ANTIN(IA1) .OR. .NOT.ANTIN(IA2)) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(13,IFNUM) = NREJCT(13,IFNUM) + 1
            GO TO 60
         END IF

C        Autocorrelations.
         IF (IA1.EQ.IA2 .AND. .NOT.DOAUTO) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(7,IFNUM) = NREJCT(7,IFNUM) + 1
            GO TO 60
         END IF

C        Baselines dropped by request.
         IF (EXBSLN(IA1,IA2)) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(8,IFNUM) = NREJCT(8,IFNUM) + 1
            GO TO 60
         END IF

C        Drop unwanted pulsar bins.
         IF (PBIN1.GT.0.AND.dm.eq.0.0) THEN
Crgd   If dm!=0 take them all
            IF (PBIN2.LE.0) THEN
              IF (BIN.NE.PBIN1) THEN
                 NREJCT(14,IFNUM) = NREJCT(14,IFNUM) + 1
                 GO TO 60
              END IF
            ELSE
Crgd Improving if statments
               if (pbin1.le.pbin2) then
                  IF (BIN.LT.PBIN1 .OR. BIN.GT.PBIN2 .OR.
     *             (PBIN3.GT.0 .AND. MOD(BIN-PBIN1,PBIN3).NE.0)) THEN
                     NREJCT(14,IFNUM) = NREJCT(14,IFNUM) + 1
                     GO TO 60
                  END IF
C Pbin2 < Pbin1 (for wrap around)
               else
                  IF ((BIN.GT.PBIN2 .and. BIN.LT.PBIN1) .OR.
     *             (PBIN3.GT.0 .AND. MOD(BIN-PBIN2,PBIN3).NE.0)) THEN
                     NREJCT(14,IFNUM) = NREJCT(14,IFNUM) + 1
                     GO TO 60
                  END IF
               endif
            END IF
         END IF

C        Apply the test for shadowing.
         IF (SHADOW.GT.0.0) THEN
            IF (TIME.NE.TSCAN2) THEN
C              New integration, reset the shadowing parameters.
               CALL SHADIN (IERR)
            END IF

            IF (SHADED(IA1) .OR. SHADED(IA2)) THEN
               NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
               NREJCT(9,IFNUM) = NREJCT(9,IFNUM) + 1
               GO TO 60
            END IF
         END IF

C        Reject baselines involving an antenna for which the XY
C        phases were bad
         IF (DOXYFL) THEN
            IF ( (USERXY .AND. (CLIPPD(IA1,IFAXID,FREQID) .OR.
     *                         CLIPPD(IA2,IFAXID,FREQID))) .OR.
     *          (.NOT.USERXY .AND. NSYSPH(IA1,IFAXID,FREQID).GT.0 .AND.
     *                             NSYSPH(IA2,IFAXID,FREQID).GT.0 .AND.
     *                            (CLIPPD(IA1,IFAXID,FREQID) .OR.
     *                             CLIPPD(IA2,IFAXID,FREQID)) ) ) THEN
               NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
               NREJCT(11,IFNUM) = NREJCT(11,IFNUM) + 1
               GO TO 60
            END IF
         END IF


C        Tell user about bad samplers.
         IF ( ((BADSS(IA1,IFNUM) .AND. .NOT.SSTDUN(IA1,IFNUM)) .OR.
     *         (BADSS(IA2,IFNUM) .AND. .NOT.SSTDUN(IA2,IFNUM))) .AND.
     *        (SSCHK.EQ.1 .OR. SSCHK.EQ.3) ) THEN

C           Announce UT of integration
            IF (.NOT.SSHEAD) THEN
               CALL TIMCON (TIME, DHMS)
               MSGTXT = ' '
               CALL ATMSG (MSGTXT)
               WRITE (MSGTXT, 130) DHMS
 130           FORMAT ('Warning: bad % sampler stats at UT: ',
     *                  I3, I3.2, ':', I2.2, ':', I2.2)
               CALL ATMSG (MSGTXT)
C
               SSHEAD = .TRUE.
            END IF

C           Report values if haven't already done this antenna
            IF (BADSS(IA1,IFNUM) .AND. .NOT.SSTDUN(IA1,IFNUM)) THEN
               WRITE (MSGTXT, 140) IA1, IFNUM,
     *            (SYSNUM(IA1,IFNUM,I),I=8,13)
 140           FORMAT ('X,Y stats for ant/IF ', I1, '/', I1, ' : ',
     *                  2(F5.1,','), F5.1, 3X, 2(F5.1,','), F5.1)
               CALL ATMSG (MSGTXT)

C              Indicate that we don't want to mention the sampler
C              stats for this antenna/IF again until the
C              next integration
               SSTDUN(IA1,IFNUM) = .TRUE.
            END IF
C
            IF (BADSS(IA2,IFNUM) .AND. .NOT.SSTDUN(IA2,IFNUM)) THEN
               WRITE (MSGTXT, 140) IA2, IFNUM,
     *            (SYSNUM(IA2,IFNUM,I),I=8,13)
               CALL ATMSG (MSGTXT)
               SSTDUN(IA2,IFNUM) = .TRUE.
            END IF
         END IF

C        Reject baselines involving an antenna with bad sampler
C        statistics if dropping or correcting (in this case,
C        BADSS is true if the samplers are so wrong they
C        can't be fixed up).
         IF ( (SSCHK.GE.2 .AND. SSCHK.LE.4) .AND.
     *        (BADSS(IA1,IFNUM) .OR. BADSS(IA2,IFNUM)) ) THEN
            NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
            NREJCT(12,IFNUM) = NREJCT(12,IFNUM) + 1
            GO TO 60
         END IF


C        Initialize correction factors (sampler errors and Tsys)
C        for visibilities
         FACXX = 1.0
         FACYY = 1.0
         FACXY = 1.0
         FACYX = 1.0

C        Find sampler statistics correction factors.  Formula only for
C        low correlation coefficients, so can't do autocorrelations
         IF (SSCHK.EQ.4 .AND. IA1.NE.IA2) THEN
            CALL SSCOR (SYSNUM(IA1,IFNUM, 8), SYSNUM(IA1,IFNUM,9),
     *                  SYSNUM(IA1,IFNUM,10),
     *                  SYSNUM(IA1,IFNUM,11), SYSNUM(IA1,IFNUM,12),
     *                  SYSNUM(IA1,IFNUM,13),
     *                  SYSNUM(IA2,IFNUM, 8), SYSNUM(IA2,IFNUM,9),
     *                  SYSNUM(IA2,IFNUM,10),
     *                  SYSNUM(IA2,IFNUM,11), SYSNUM(IA2,IFNUM,12),
     *                  SYSNUM(IA2,IFNUM,13),
     *                  TRUPOL, SSEXP, FACXX, FACYY, FACXY, FACYX,
     *                  SSFIX, SSFAC, SSMN, SSMX)
            IF (SSFIX) THEN
               NSSFIX(IFNUM) = NSSFIX(IFNUM) + 1
               SSSUM(IFNUM) = SSSUM(IFNUM) + SSFAC
C
               SSMIN(IFNUM) = MIN(SSMIN(IFNUM), SSMN)
               SSMAX(IFNUM) = MAX(SSMAX(IFNUM), SSMX)
            END IF
         END IF

C        Find Tsys correction factors
         IF (LOCDOT.EQ.-1 .OR. LOCDOT.GT.1) THEN
C
C           Fudge Tsys for IF 2 with IF 1 value
            AXEUSE = IFAXID
            IF (TSYFDG) AXEUSE = 1

            IF (NSYSTX(IA1,AXEUSE,FREQID,SOUID).GT.0 .AND.
     *          NSYSTX(IA2,AXEUSE,FREQID,SOUID).GT.0 .AND.
     *          NSYSTY(IA1,AXEUSE,FREQID,SOUID).GT.0 .AND.
     *          NSYSTY(IA2,AXEUSE,FREQID,SOUID).GT.0) THEN
               CALL TSYSCO (LOCDOT, TRUPOL,
     *            SYSNUM(IA1,IFNUM,5), SYSNUM(IA2,IFNUM,5),
     *            SYSNUM(IA1,IFNUM,6), SYSNUM(IA2,IFNUM,6),
     *            TXAVE(IA1,AXEUSE,FREQID,SOUID),
     *            TXAVE(IA2,AXEUSE,FREQID,SOUID),
     *            TYAVE(IA1,AXEUSE,FREQID,SOUID),
     *            TYAVE(IA2,AXEUSE,FREQID,SOUID),
     *            FACXX, FACYY, FACXY, FACYX, IERR)
               IF (IERR.NE.0) GO TO 999
            ELSE
               WRITE (MSGTXT, 150)
     *            SU_NAME(SRCNUM)(1:ITRIM(SU_NAME(SRCNUM))),
     *            IA1, IA2
 150           FORMAT ('WARNING! Drop ', A, ', ', I1, '-', I1,
     *                 ' until syscal found')
               CALL ATMSG (MSGTXT)

C              Tsys correction impossible.
               NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
               NREJCT(10,IFNUM) = NREJCT(10,IFNUM) + 1
               GO TO 60
            END IF
         END IF

C        Apply sampler and Tsys corrections to data
         CALL CORAPP (TRUPOL, FACXX, FACYY, FACXY, FACYX,
     *                NUMFRQ, VIS(BPOINT))

C        Apply Hanning smoothing
         IF (HANN) CALL HSM (IF_NFREQ(IFNUM), IF_NSTOK(IFNUM),
     *                       VIS, VIS2)

C        Convert to Stokes parameters if required.
C        Integrity of Stokes type checked in POLDEC
         IF (DOSTOK .OR. YGNROT) THEN
            IF ( (NSYSPH(IA1,IFAXID,FREQID).GT.0 .AND.
     *            NSYSPH(IA2,IFAXID,FREQID).GT.0) .OR. USERXY) THEN

C              Catch baseline renaming convention change
               IF (JDAY.LT.JULBAS) THEN
                  STKA1 = IA2
                  STKA2 = IA1
               ELSE
                  STKA1 = IA1
                  STKA2 = IA2
               END IF

C              Use user specified XY phases as desired.  Do this for
C              each visibility because of FREQID and IFAXID assignments
               IF (USERXY) THEN
                  DO 160 I = 1, NCA
                     PHSAVE(I,IFAXID,FREQID)
     *                      = XYPHAS(I,XYPHPT(IFAXID,FREQID))
 160              CONTINUE
               END IF

C              Convert to Stokes or rotate the Y gain phase
               IF (DOSTOK) THEN
                  CALL LINSTK (TRUPOL, NUMFRQ,
     *                         SYSNUM(STKA1,IFNUM,4),
     *                         SYSNUM(STKA2,IFNUM,4),
     *                         SYSSGN*PHSAVE(STKA1,IFAXID,FREQID),
     *                         SYSSGN*PHSAVE(STKA2,IFAXID,FREQID),
     *                         VIS(BPOINT))
               ELSE IF (YGNROT) THEN
                  CALL LINROT (TRUPOL, NUMFRQ,
     *                         SYSSGN*PHSAVE(STKA1,IFAXID,FREQID),
     *                         SYSSGN*PHSAVE(STKA2,IFAXID,FREQID),
     *                         VIS(BPOINT))
               END IF
            ELSE
               WRITE (MSGTXT, 150) IA1, IA2
               CALL ATMSG (MSGTXT)

C              Stokes conversion impossible.
               NSUVIS(SRCNUM,IFNUM,2) = NSUVIS(SRCNUM,IFNUM,2) + 1
               NREJCT(10,IFNUM) = NREJCT(10,IFNUM) + 1
               GO TO 60
            END IF
         END IF

C        Take complex conjugate if sideband inverted
         SGN = REAL(IF_INVERT(IFNUM))

C        Deal with autocorrelations and reversed baselines
         IF (IA1.EQ.IA2) THEN
C           Autocorrelation. Ensure (u,v,w) = (0,0,0).
            U = 0.0
            V = 0.0
            W = 0.0
         ELSE IF (IA1.GT.IA2) THEN
C           Reversed baseline, change direction and negate phase.
            BASELN = 256*IA2 + IA1
            IA1 = BASELN/256
            IA2 = BASELN - 256*IA1
            U = -U
            V = -V
            W = -W
            SGN = -SGN
         END IF

C        Negate phase if requested by user
         IF (DONPHA) SGN = -SGN

C        If the current non-rejected baseline comes from a new inte-
C        gration, write out the accumulated data from the previous
C        integration and reinitialize for the next integration
C        Catch data out of time order as well here.
C        Read in time-binned data
         IF (ABS(UT-OLDUT).GT.0.05) THEN
C..         IF (ABS(UT-OLDUT).GT.TRESLN) THEN
            CALL UVDUMP (NBASE, NZFILL, OLDUT, IERR)
            IF (IERR.NE.0) GOTO 999
         END IF

C        Do indexing.
         IF (TBSORT) THEN
            IF (TIME.LT.TSCAN2) THEN
C              This visibility is not in time order!
               CALL FIXSRT (JERR)
C
               CALL TIMCON (TIME, DHMS)
               WRITE (MSGTXT, 165) DHMS
 165           FORMAT ('ATUV: DATA OUT OF TIME ORDER. CURRENT UT = ',
     *                 I3, I3.2, ':', I2.2,':', I2.2)
               CALL ATMSG (MSGTXT)
               NSEVER = MIN(NSEVER+1, MAXSEV)
               SEVERE(NSEVER) = MSGTXT
            ELSE
               NEWNDX = .FALSE.
               IF (NVIS.EQ.0) THEN
C                 First visibility of this scan.
                  NEWNDX = .TRUE.
               ELSE IF (FREQID.NE.PREVFQ .OR. SOUID.NE.PREVSU .OR.
     *            TIME-TSCAN2.GT.NXGAP .OR. TIME-TSCAN1.GT.NXSPAN) THEN
C                 Time for a new NX entry.
                  CALL ATNXCL (NANT, PREVFQ, PREVSU, JERR)
                  IF (JERR.NE.0) THEN
                     MSGTXT = 'ATUV: ERROR UPDATING NX AND CL TABLES.'
                     CALL ATMSG (MSGTXT)
                     CALL FIXSRT (JERR)
                  END IF
                  NEWNDX = .TRUE.
               END IF

               IF (NEWNDX) THEN
C                 Start a new scan index entry.
                  PREVFQ = FREQID
                  PREVSU = SOUID
                  TSCAN1 = TIME
                  VSCAN1 = VSCAN2 + 1

                  DO 170 IANT = 1, NCA
                     SCANTN(IANT) = .FALSE.
 170              CONTINUE
               END IF

C              Increment visibility index for end of NX entry if this
C              baseline has not yet been encountered in this integration
               IF (BASIND(BASELN).EQ.0) VSCAN2 = VSCAN2 + 1

               TSCAN2 = TIME
               SCANTN(IA1) = .TRUE.
               SCANTN(IA2) = .TRUE.
            END IF
         END IF


C        Set baseline conversion arrays
         IF (BASIND(BASELN).EQ.0) THEN

C           This is a new baseline this integration so increment
C           the baseline index for this integration
            NBASE = NBASE + 1

C           Convert from baseline number to scan baseline index
            BASIND(BASELN) = NBASE

C           Convert from scan baseline index to baseline number
            INDBAS(NBASE) = BASELN

C           Increment local scan and total visibility counters
            NVIS = NVIS + 1
            VISCNT = VISCNT + 1
         END IF

C        Assign the baseline index for this integration
         IBASE = BASIND(BASELN)

C        Increment the counter for this baseline and IF in this
C        integration (i.e., number of accumulated bins).
         BASAX(IBASE,IFAXID) = BASAX(IBASE,IFAXID) + 1

C        Update the source/frequency accepted counter
         NSUVIS(SRCNUM,IFNUM,1) = NSUVIS(SRCNUM,IFNUM,1) + 1

C        Need we expand the uv file?
         IF (VISCNT.GT.CATBLK(KIGCN)) THEN

C           Provide for another batch of visibilities.
            NBYPR = LREC * 2
            NREC  = NBLKS

            NXPND = MIN(1000, NBPS*(1000 - 2)/NBYPR)
            NXPND = MAX(100, NXPND)
            CATBLK(KIGCN) = CATBLK(KIGCN) + NXPND

            XBLKS = CATBLK(KIGCN)
            XBYPR = NBYPR
            XBPS = NBPS
            XBLKS = XBLKS * XBYPR + XBPS - 1.0D0
            XBLKS = XBLKS / XBPS + 2.0D0
            NBLKS = XBLKS + 0.001D0
            NREC  = NBLKS - NREC
            CALL ZEXPND (UVLUN, OUTDSK, UVNAME, NREC, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ATUV: FILE EXPANSION FAILED - TRANSFER ' //
     *                  'NOT COMPLETED.'
               CALL ATMSG (MSGTXT)
               GO TO 999
            END IF
         END IF


C        Accumulate this visibility in the big buffer
         CALL UVACUM (NUMFRQ, NUMPOL, TIME, IFAXID, FREQID,
     *                SOUID, BPOINT, SGN)

C        Go and get another visibility
      GO TO 60


 999  RETURN
      END



      SUBROUTINE UVACUM (NUMFRQ, NUMPOL, TIME, IFAXID, FREQID,
     *                   SOUID, BPOINT, SGN)
C----------------------------------------------------------------------
C     Accumulate the current visibility into a big buffer which
C     will hold all the data for one integration
C
C     Input:
C        NUMFRQ   I    Number of channels encompassing all selected
C                      channels (ECHAN-BCHAN+1)
C        NUMPOL   I    Number of polarizations to write out
C        IFAXID   I    Location on AIPS IF axis for this visibiltiy
C        FREQID   I    Frequency id number in FQ table
C        SOUID    I    SOurce id number in SU table
C        BPOINT   I    Pointer to first location in complex VIS
C                      buffer that we want to keep
C        SGN      R    Multiply phase by this sign
C        TIME     D    Time of visibility; offset days from first
C                      day in output file.
C
C     Common input/output:
C        BASIND   I    Baseline number (e.g. 258) to baseline index
C        RANDUN   L    True for baselines that have already had
C                      random parameters loaded
C        UVBUFF   R    Accumulation buffer for data
C        BASAX    I    Number of (pulsar) bins processed on each
C                      baseline and IF for this integration.
C
C
C     Common input:
C        NUMIF    I    Length of IF axis
C        REFREQ   D    Reference frequency
C        LREC2    I    Number of words in a visibility assuming it is
C                      uncompressed and including random parameters
C        IBASE    I    Baseline index (1 .. NBASE) for this baseline
C                      in this integration. Each baseline counts
C                      just once, regardless of how many simultaneous
C                      frequencies it may gave in each integration
C        U,V,W    R    uvw values for  visibility
C        BASELN   I    Baseline number of visibility
C        VIS      C    Complex visiblities
C        DOUVCM   L    True for compressed data
C        CHANSL   I    Groups of select channels
C        WCHNSL   R    Array of weights for each channel in visiblity
C----------------------------------------------------------------------
      INTEGER IFAXID, FREQID, SOUID, BPOINT, NUMPOL, NUMFRQ
      DOUBLE PRECISION TIME
      REAL SGN
C
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C
      INTEGER BASPNT, L, M, IFOFF
      DOUBLE PRECISION LAMBDA
      REAL WT, fraction
Crgd >
C     integer ichan_offset
      integer ifrq, i_tmp
      real    weight_tmp, refpix
      DOUBLE PRECISION FRQNCY, FRQINC, dt
Crgd <

      DOUBLE PRECISION C
      PARAMETER (C = 2.99792458D8)
C----------------------------------------------------------------------
C     Compute pointer for the start of this baseline in accumulation
C     buffer (which is always uncompressed).  This is the location
C     for this baseline for the first location of the IF axis.
      BASPNT = (BASIND(BASELN)-1) * LREC2 + 1

Crgd >
      ifrq=ifaxid
C   OR should this be freqid?
      if (dm.ne.0.0) then
         refpix = IF_REF(ifrq)
         frqncy = IF_FREQ(ifrq)
         IF (NUMFRQ.EQ.1) THEN
            FRQINC = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))
         ELSE
C   c.f. RPFITS definition of bandwidth
            FRQINC = IF_INVERT(IFRQ)*ABS(IF_BW(IFRQ))/(NUMFRQ-1)
         END IF
       endif
Crgd <

C     Load random parameters for this baseline if not done yet
      IF (.NOT.RANDUN(IBASE)) THEN
         LAMBDA = C/REFREQ
         UVBUFF(BASPNT) = U/LAMBDA
         UVBUFF(BASPNT+1) = V/LAMBDA
         UVBUFF(BASPNT+2) = W/LAMBDA
         UVBUFF(BASPNT+3) = BASELN
         UVBUFF(BASPNT+4) = REAL(TIME)
         UVBUFF(BASPNT+5) = SOUID
         UVBUFF(BASPNT+6) = FREQID
C
         RANDUN(IBASE) = .TRUE.
      END IF

C     Compute IF axis offset for accumulation buffer
      IFOFF  = ((IFAXID-1) * NAXVL2 / NUMIF) + 7

C     See if we have integration times to set weights with
      IF (XDPARM(3).GT.0.0) THEN
         WT = XDPARM(3)
      ELSE
         IF (INTBASE.GT.0.0) THEN
            WT = INTBASE / 15.0
         ELSE IF (INTIME.GT.0) THEN
            WT = REAL(INTIME) / 15.0
         ELSE
            WT = 1.0
         END IF
      END IF
      AVGWT = AVGWT + WT
      NWT = NWT + 1.0

C     Fill accumulation buffer, looping over all channels and polarizations.
C     To improve speed slightly each of 4 possible cases is treated
C     separately (removing conditionals from inside the loop).
C     Specifically, treat the first pulsar bin in accumulation separately
C     and secondly, check for the special case of regular channel flagging
C     forcing a change in the frequency increment of the output file.
C
      M = BASPNT + IFOFF

C       If only one bin is being accumulated
      IF (BASAX(IBASE,IFAXID).LE.1) THEN
         IF (NCHNSL.EQ.1 .AND. CHANSL(3,1).GT.1) THEN
C       Can accumulate this simple case more efficiently
            DO 100 L = BPOINT, BPOINT+(NUMFRQ*NUMPOL)-1
               IF (WCHNSL(L).GT.0.0) THEN
                  UVBUFF(M)   = REAL(VIS(L))
                  UVBUFF(M+1) = SGN*AIMAG(VIS(L))
                  UVBUFF(M+2) = WT

                  M = M + 3
               END IF
 100        CONTINUE
         ELSE

            DO 200 L = BPOINT, BPOINT+(NUMFRQ*NUMPOL)-1
               UVBUFF(M)   = REAL(VIS(L))
               UVBUFF(M+1) = SGN*AIMAG(VIS(L))
C              Unselected channels are flagged rather than omitted.
               UVBUFF(M+2) = WCHNSL(L)*WT
 200        M = M + 3
         END IF

      ELSE
C       If more than one bin is being accumilated
         IF (NCHNSL.EQ.1 .AND. CHANSL(3,1).GT.1) THEN
C       Can accumulate this simple case more efficiently
            DO 300 L = BPOINT, BPOINT+(NUMFRQ*NUMPOL)-1
               IF (WCHNSL(L).GT.0.0) THEN
                  UVBUFF(M)   = UVBUFF(M)   + REAL(VIS(L))
                  UVBUFF(M+1) = UVBUFF(M+1) + SGN*AIMAG(VIS(L))
                  UVBUFF(M+2) = UVBUFF(M+2) + WT

                  M = M + 3
               END IF
 300        CONTINUE

         ELSE
Crgd    Normal case.
Crgd BTW  wchnsl is always -1 or 1
Crgd    here for tracing channel as a function of bin
            DO 400 L = BPOINT, BPOINT+(NUMFRQ*NUMPOL)-1
               weight_tmp=0.0
Crgd As just in case - if all these are missed it is rejected.
C              IF (WCHNSL(L).LT.0) THEN
Crgd - for rejected channels I am taking all bins - Is this right?
Crgd Does it matter?
C                  weight_tmp=WCHNSL(L)
C               ELSE
               IF (dm.NE.0.0.AND.period.NE.0.0) THEN
Crgd want to select channels from other bins if the delays are right
                  ifrq=int((1.0*(L-BPOINT))/NUMPOL)-refpix
                  dt=4.15D15*dm*((frqncy+frqinc*ifrq)**(-2)
     :                 -frqncy**(-2))
Crgd                  ichan_offset=BIN+nint(fraction)
                  fraction=BIN+dt/period*nbins

Crgd   Shift back into the selected range
                  do while (fraction.LT.0)
                     fraction=fraction+nbins
                  end do
                  do while (fraction.GT.nbins+1)
                     fraction=fraction-nbins
                  end do
C                  do while (ichan_offset.LT.1)
C                     ichan_offset=ichan_offset+nbins
C                  end do
C                  do while (ichan_offset.GT.nbins)
C                     ichan_offset=ichan_offset-nbins
C                  end do

Crgd Attempt better weights. 1st is it close?
                  if (pbin1.le.pbin2) then
                     if (pbin1.eq.1.and.fraction.gt.nbins) then
                        fraction=fraction-nbins
                     else if (pbin2.eq.nbins.and.fraction.lt.1) then
                        fraction=fraction+nbins
                     endif
                     if (pbin3.le.1) then
                        if (pbin1.eq.1.and.pbin2.eq.nbins) then
Crgd  requested all channels
                           weight_tmp=1.0
                        else if (fraction-pbin1.gt.-1.0) then
Crgd Above pbin1-1
                           weight_tmp=fraction-(pbin1-1)
                           if (weight_tmp.gt.1.0) then
Crgd And Below pbin2+1    [pbin2-fraction.gt.-1.0]
                              weight_tmp=(pbin2+1)-fraction
C                           else
C                              weight_tmp=0.0
                           endif
                        endif
                     else
C   HERE
                        do 500 i_tmp=pbin1,pbin2,pbin3
                           if (abs(fraction-i_tmp).lt.1.0) then
                              weight_tmp=1.0-abs(fraction-i_tmp)
                              goto 501
                           endif
 500                    continue
 501                    continue
                     endif
                  else
Crgd pbin2<pbin1
                     if (pbin3.le.1) then
Crgd Crazy case dm!=0 but requested all channels from n to n-1
                        if (pbin1-pbin2.eq.1) then
                           weight_tmp=1.0
Crgd just above pbin2
                        else if (fraction-pbin2.lt.1.0) then
                           weight_tmp=(1+pbin2)-fraction
                        else if (pbin1-fraction.lt.1.0) then
Crgd Or just below pbin1
                           weight_tmp=fraction-(pbin1-1)
                        else
                           weight_tmp=0.0
                        endif
                     else
C HERE
                        do 600 i_tmp=
     :                      (mod(pbin1-nbins,pbin3)+pbin3),pbin2,pbin3
                           if (abs(fraction-i_tmp).lt.1.0) then
                              weight_tmp=1.0-abs(fraction-i_tmp)
                              goto 602
                           endif
 600                    continue
                        do 601 i_tmp=pbin1,nbins,pbin3
                           if (abs(fraction-i_tmp).lt.1.0) then
                              weight_tmp=1.0-abs(fraction-i_tmp)
                              goto 602
                           endif
 601                    continue
 602                    continue
                     endif
                  endif
                  if (weight_tmp.gt.1.0) weight_tmp=1.0
                  if (weight_tmp.lt.0.0) weight_tmp=0.0
                  weight_tmp=weight_tmp*wchnsl(l)

CCrgd   Select those bins asked for (at ref freq)
C                  if (pbin1.le.pbin2) then
C                     if (ichan_offset.GE.PBIN1.AND.
C     :                    ichan_offset.LE.PBIN2) then
CCrgd               between the first 2 pars
C                        if (PBIN3.NE.0.AND.MOD(ichan_offset-PBIN1,PBIN3)
C     :                       .NE.0) then
CCrgd                  but  not on the 3rd
C                           weight_tmp=0.0
C                        else
CCrgd                  At last we accept something.
C                           weight_tmp=WCHNSL(L)
C                        endif
C                     else
CCrgd Not between requested bins
C                        weight_tmp=0.0
C                     endif
C                  else
C                     if (ichan_offset.GE.PBIN1.OR.ichan_offset.LE.PBIN2)
C     :                    then
CCrgd               between the first 2 pars
C                        if (PBIN3.NE.0.AND.MOD(ichan_offset-PBIN2,PBIN3)
C     :                       .NE.0)then
CCrgd                  but  not on the 3rd
C                           weight_tmp=0.0
C                        else
CCrgd                  At last we accept something.
C                           weight_tmp=WCHNSL(L)
C                        endif
C                     else
CCrgd Not between requested bins
C                        weight_tmp=0.0
C                     endif
C                  endif
               ELSE
Crgd            normal no dedispersion case
                  WEIGHT_TMP=WCHNSL(L)
               END IF
Crgd Only add if wanted. Does allow wchnsl(l) < 0 to be included.
               if (weight_tmp.ne.0.0) then
                  UVBUFF(M)   = UVBUFF(M)   + REAL(VIS(L))*weight_tmp
                  UVBUFF(M+1) = UVBUFF(M+1) +
     :                 SGN*AIMAG(VIS(L))*weight_tmp
                  UVBUFF(M+2) = UVBUFF(M+2) + WT*weight_tmp
               end if
 400        M = M + 3
         END IF
      END IF

      RETURN
      END



      SUBROUTINE UVDUMP (NBASE, NZFILL, OLDUT, IERR)
C----------------------------------------------------------------------
C     Deposit the contents of the current accumulation buffer, for
C     the previous integration, to disk.
C
C   Input/output:
C      NBASE   I    Number of baselines in this integration
C                   Zeroed on output
C      NZFILL  I    Number of visibilities zero filled per IF for scan
C
C      OLDUT   R    On input, is the UT of the integration about
C                   to be written to disk.  On output, is the UT
C                   of the current integration
C   Output:
C      IERR    I    0 => OK
C
C   Common input:
C      NUMIF   I    Length of IF axis
C      NCA     I    Number of antennas in CA
C      UT      R    UT of current baseline
C      UVLUN   I    LUN of output file
C      UVFIND  I    FTAB pointer for UV file
C      NAXVAL  I    Number of words in a visibility on disk
C                   excluding random parameters
C      NAXVL2  I    Number of words in a visibility assuming it is
C                   uncompressed and excluding random parameters
C      LREC    I    Number of words in a visibility on disk
C                   including random parameters
C      LREC2   I    Number of words in a visibility assuming it is
C                   uncompressed and including random parameters
C      DOUVCM  L    True to write compressed visibilities
C
C   Common input/output:
C      IBASE   I    Baseline index. Zeroed on output
C      BASIND  I    Baseline number (e.g. 258) to baseline index
C                   Zeroed on output
C      INDBAS  I    Baseline index to baseline number
C                   Zeroed on output
C      BASAX   I    Number of (pulsar) bins processed on each
C                   baseline and IF for this integration
C                   Reset to zero on output.
C      RANDUN  L    True if random parameters loaded for each IF
C                   Set to false on output
C      UVBUFF  R    The buffer containing the accumulated data
C                   from the integration about to be output
C      UVOUT   R    The buffer into which the data are copied
C                   or compressed for output to disk.
C      UVBIND  I    Points at the next available location in
C                   UVOUT for data
C      NPIO    I    Maximum number of visibilities that can be
C                   transferred in one call to UVDISK
C      NIO     I    Number of visibilties accumulated  in
C                   output buffer UVOUT so far.  Will be zero
C                   output if UVDISK is called.
C      OVFLOW  I    Number of overflowed visiblities when
C                   compressing data
C      OVFLIM  R    Maximum value a correlation can have before
C                   it gets clipped under compression (input only).
C
C   Notes:
C       1)  Because UVDISK transfers integral sectors, it is generally
C           impossible to write out the entire integration. Some of
C           it is left over for next time.  The very last call to
C           UVDISK (done in ATCLOS) flushes out the non-integral
C           sector amounts of data.   The pointer UVBIND is used to
C           deal with the bits of data left over from last time.
C
C----------------------------------------------------------------------
      INTEGER IERR, NZFILL(*), NBASE
      REAL OLDUT
C
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
CC
      INTEGER I, J, K, M, BASPNT, IFOFF, NAVG
      REAL AVGAMP, AMP, AMPMAX, WTSCL(2)
C----------------------------------------------------------------------
      IERR = 0
      IF (NBASE.EQ.0) THEN

C        Just update UT of previous integration if it had no
C        accepted data, or if this is the first time
         OLDUT = UT
         GOTO 999
      END IF

C     Re-compute UVWs, if required.
      IF (REDOUV) CALL UVWCAL (NBASE,IERR)

C     Loop over accumulated data in all baselines
      DO 100 I = 1, NBASE

C        Find pointer for start of vis. in accumulation buffer
         BASPNT = (I-1) * LREC2 + 1

C        Loop over length of IF axis
         DO 20 J = 1, NUMIF
            IFOFF  = ((J-1) * NAXVL2 / NUMIF) + 7

C           See if this IF location has any data for this baseline
            IF (BASAX(I,J).LE.0) THEN

C              Increment zero fill counter
               NZFILL(J) = NZFILL(J) + 1

C              Fill record with zeros; weights zero too so
C              can't be unflagged
               M = BASPNT + IFOFF
               DO 10 K = 1, NAXVL2/NUMIF
                  UVBUFF(M) = 0.0
 10            M = M + 1

            ELSE IF (BASAX(I,J).GT.1) THEN
C              Re-scale if more than one (pulsar) bin combined.
               M = BASPNT + IFOFF
               DO 15 K = 1, NAXVL2/NUMIF
                  UVBUFF(M) = UVBUFF(M)/BASAX(I,J)
 15            M = M + 1
            END IF


C     Increment overflow counters
            IF (DOUVCM) THEN
               NAVG = 0
               AMPMAX = 0.0
               AVGAMP = 0.0

C              Loop over all unflagged visibilities for this IF/Baseline
               M = BASPNT + IFOFF
               DO 18 K = 1, NAXVL2/NUMIF, 3
                  IF (UVBUFF(K+2).GT.0.0) THEN
                     NAVG = NAVG + 1
                     AMP = SQRT(UVBUFF(M)**2 + UVBUFF(M+1)**2)
                     AMPMAX = MAX(AMPMAX, AMP)
                     AVGAMP = AVGAMP + AMP
                  END IF
 18            M = M + 3

               IF (NAVG.GT.0 .AND. NAVG*AMPMAX.GT.OVFLIM*AVGAMP)
     *            OVFLOW = OVFLOW + 1
            END IF

 20      CONTINUE

C        Compress data if desired into output buffer
         IF (DOUVCM) THEN

C           Compress
            CALL ZUVPAK (NAXVAL, UVBUFF(BASPNT+7), WTSCL,
     *                   UVOUT(UVBIND+9))

C           Copy random parameters and add the scale factors
            DO 50 J = 1, 7
               UVOUT(UVBIND+J-1) = UVBUFF(BASPNT+J-1)
 50         CONTINUE
            UVOUT(UVBIND+7) = WTSCL(1)
            UVOUT(UVBIND+8) = WTSCL(2)
         ELSE

C           Copy data into output buffer
            CALL RCOPY (LREC, UVBUFF(BASPNT), UVOUT(UVBIND))
         END IF

C        Write out accumulated visibilities to disk if time for I/O
C        UVDISK transfers NIO visibilities at a time.
         NIO = NIO + 1
         UVBIND = UVBIND + LREC

         IF (NIO.GE.NPIO) THEN
            CALL UVDISK ('WRIT', UVLUN, UVFIND, UVOUT, NIO,
     *                    UVBIND, IERR)
C
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 90) IERR
 90            FORMAT ('UVDUMP: ERROR ', I2,
     *                 ' WRITING VISIBILITIES TO DISK')
               CALL ATMSG (MSGTXT)
               GO TO 999
            END IF
C
            NPIO = NIO
            NIO = 0
         END IF

 100  CONTINUE

C     Reinitialize integration based masks
      DO 110 I = 1, NBASE
         BASIND(INDBAS(I)) = 0
         RANDUN(I) = .FALSE.
         INDBAS(I) = 0
         DO 105 J = 1, NUMIF
            BASAX(I,J) = 0
 105     CONTINUE
 110  CONTINUE
      NBASE = 0
      IBASE = 0

C     Update UT of integration just dumped to disk
      OLDUT = UT
C
 999  RETURN
      END



      SUBROUTINE REPORT (NSUVIS, MAXREJ, NREJCT, NZFILL, NSSFIX,
     *                   SSSUM, SSMIN, SSMAX)
C-----------------------------------------------------------------------
C     REPORT provides a summary of the scan just read.
C
C     Given:
C          NSUVIS(MAX_SU,MAX_IF,2)
C                        I     Number of visibilities accepted and
C                              rejected for each source.
C          NREJCT(MAXREJ,MAX_IF)
C                        I     Number of visibilities rejected in this
C                              scan, sorted by category as defined in
C                              subroutine ATUV.  Sorted by IF too.
C          NZFILL(MAX_IF)I    Number of zero filled visibilities
C                             for each IF axis location
C          NSSFIX(MAX_IF)I    Number of visibilities that were
C                             corrected for bad samplers
C          SSSUM(MAX_IF) R    Sum of average sampler corrections
C          SSMIN,MAX(MAX_IF)
C                        R    Min and max sampelr corrections
C
C     Given via RPFITS common IF:
C          IFNUMS(MAX_IF)
C                        I     RPFITS IF numbers corresponding to the
C                              IFNUM random parameter for this scan.
C          IF_FREQ(MAX_IF)
C                        D     Reference frequency (usually the centre
C                              frequency) of each band in Hz.
C          IF_INVERT(MAX_IF)
C                        I     Is -1 if the video is inverted, +1
C                              otherwise.
C
C     Given via RPFITS common SU:
C          SUNUM(MAX_SU)
C                        I     Source numbers corresponding to the
C                              SRCNUM random parameter for this scan.
C          SU_NAME(MAX_SU)
C                        C*16  Source names.
C
C     Called:
C          ATLOD:  {ATMSG}
C          APLNOT: {STRIM}
C
C
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'

      INTEGER   IFSEL(MAX_IF), IFRQ, ISRC, J, K, L,
     *          N, NFRQ, NSRC, NSUVIS(MAX_SU,MAX_IF,2), MAXREJ,
     *          NREJCT(MAXREJ,MAX_IF), NZFILL(MAX_IF), NSSFIX(MAX_IF)
      REAL SSSUM(MAX_IF), SSMIN(MAX_IF), SSMAX(MAX_IF)
      LOGICAL NONE

      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)

C  Report on each source at each frequency.
C     Were there any sources in this scan?
      DO 20 IFRQ = 1, MAX_IF
         DO 10 ISRC = 1, MAX_SU
            IF (NSUVIS(ISRC,IFRQ,1).GT.0) GO TO 50
            IF (NSUVIS(ISRC,IFRQ,2).GT.0) GO TO 50
 10      CONTINUE
 20   CONTINUE

      MSGTXT = 'No visibilities in this scan.'
      CALL ATMSG (MSGTXT)

C     Any rejects of any kind?
      DO 40 IFRQ = 1, MAX_IF
         DO 30 J = 1, MAXREJ
            IF (NREJCT(J,IFRQ).GT.0) GO TO 120
 30      CONTINUE
 40   CONTINUE


      GO TO 999

 50   MSGTXT = 'Visibilities accepted/rejected for each ' //
     *         'source/frequency:'
      CALL ATMSG (MSGTXT)

C     Which IFs have non-zero entries in the NSUVIS table?
      NFRQ = 0
      NSRC = 0
      DO 70 IFRQ = 1, MAX_IF
         NONE = .TRUE.
         DO 60 ISRC = 1, MAX_SU
            IF (NSUVIS(ISRC,IFRQ,1).GT.0 .OR.
     *         NSUVIS(ISRC,IFRQ,2).GT.0) THEN
               NONE = .FALSE.
               NSRC = MAX(ISRC,NSRC)
            END IF
 60      CONTINUE
         IF (.NOT.NONE) THEN
            NFRQ = NFRQ + 1
            IFSEL(NFRQ) = IFRQ
         END IF
 70   CONTINUE

C     Write out the source specific summary.
      DO 110 K = 1, NFRQ, 4
         L = MIN(K+3,NFRQ)

         WRITE (MSGTXT, 75) (IF_INVERT(IFRQ), IFRQ=K,L)
 75      FORMAT ('   Sideband             ',4(SP,I3,9X))
         CALL ATMSG (MSGTXT)

         WRITE (MSGTXT, 77) (SELAX(IFRQ), IFRQ=K,L)
 77      FORMAT ('   AIPS IF axis         ', 4(SP,I3,9X))
         CALL FIXIFL (MSGTXT)
         CALL ATMSG (MSGTXT)

         WRITE (MSGTXT, 80) (IF_FREQ(IFSEL(IFRQ))/1E9, IFRQ=K,L)
 80      FORMAT ('   Source/Freq(GHz)',4(F11.6,:))
         CALL ATMSG (MSGTXT)

         DO 100 ISRC = 1, NSRC
            L = MIN(K+3,NFRQ)
            WRITE (MSGTXT, 90) SU_NAME(ISRC),
     *         ((NSUVIS(ISRC,IFSEL(IFRQ),J), J=1,2), IFRQ=K,L)
 90         FORMAT (3X,A,1X,4(I6,'/',I5,:))
            CALL STRIM (MSGTXT(28:32), N)
            CALL STRIM (MSGTXT(40:44), N)
            CALL STRIM (MSGTXT(52:56), N)
            CALL STRIM (MSGTXT(64:68), N)
            CALL ATMSG (MSGTXT)
 100     CONTINUE
 110  CONTINUE


C  Report rejects by category.
 120  MSGTXT = 'Category summaries of visibilities for this scan:'
      CALL ATMSG (MSGTXT)

      IF (NREJCT(1,1).GT.0) THEN
         WRITE (MSGTXT, 130) NREJCT(1,1)
 130     FORMAT (I6,' REJECTED  with invalid random parameters')
         CALL ATMSG (MSGTXT)
      END IF

      IF (NREJCT(4,1).GT.0) THEN
         WRITE (MSGTXT, 140) NREJCT(4,1)
 140     FORMAT (I6,' REJECTED  by frequency selection')
         CALL ATMSG (MSGTXT)
      END IF


      DO 160 J = 1, MAX_IF
         IF (NREJCT(2,J).GT.0) THEN
            WRITE (MSGTXT, 150) NREJCT(2,J), IF_FREQ(J)/1E9
 150        FORMAT (I6,' PROCESSED as system calibration records at ',
     *              F8.5, ' GHz')
         CALL ATMSG (MSGTXT)
         END IF
 160  CONTINUE

      DO 170 J = 1, MAX_IF
         IF (NREJCT(3,J).GT.0) THEN
            WRITE (MSGTXT, 165) NREJCT(3,J), IF_FREQ(J)/1E9
 165        FORMAT (I6,' REJECTED  by on-line flagging at           ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 170  CONTINUE

      DO 190 J = 1, MAX_IF
         IF (NREJCT(5,J).GT.0) THEN
            WRITE (MSGTXT, 180) NREJCT(5,J), IF_FREQ(J)/1E9
 180        FORMAT (I6,' REJECTED  by source selection at           ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 190  CONTINUE

      DO 210 J = 1, MAX_IF
         IF (NREJCT(6,J).GT.0) THEN
            WRITE (MSGTXT, 200) NREJCT(6,J), IF_FREQ(J)/1E9
 200        FORMAT (I6,' REJECTED  by time range selection at       ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 210  CONTINUE

      DO 230 J = 1, MAX_IF
         IF (NREJCT(7,J).GT.0) THEN
            WRITE (MSGTXT, 220) NREJCT(7,J), IF_FREQ(J)/1E9
 220        FORMAT (I6,' REJECTED  as autocorrelations at           ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 230  CONTINUE

      DO 250 J = 1, MAX_IF
         IF (NREJCT(8,J).GT.0) THEN
            WRITE (MSGTXT, 240) NREJCT(8,J), IF_FREQ(J)/1E9
 240        FORMAT (I6,' REJECTED  by baseline selection at         ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 250  CONTINUE

      DO 270 J = 1, MAX_IF
         IF (NREJCT(9,J).GT.0) THEN
            WRITE (MSGTXT, 260) NREJCT(9,J), IF_FREQ(J)/1E9
 260        FORMAT (I6,' REJECTED  by shadowing at                  ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 270  CONTINUE

      DO 290 J = 1, MAX_IF
         IF (NREJCT(10,J).GT.0) THEN
            WRITE (MSGTXT, 280) NREJCT(10,J), IF_FREQ(J)/1E9
 280        FORMAT (I6,' REJECTED  owing to lack of syscal data at  ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 290  CONTINUE

      DO 310 J = 1, MAX_IF
         IF (NREJCT(11,J).GT.0) THEN
            WRITE (MSGTXT, 300) NREJCT(11,J), IF_FREQ(J)/1E9
 300        FORMAT (I6,' REJECTED  with bad XY phase difference at  ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 310  CONTINUE

      DO 330 J = 1, MAX_IF
         IF (NREJCT(12,J).GT.0) THEN
            WRITE (MSGTXT, 320) NREJCT(12,J), IF_FREQ(J)/1E9
 320        FORMAT (I6,' REJECTED  with bad sampler statistics at   ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 330  CONTINUE


      DO 350 J = 1, MAX_IF
         IF (NREJCT(13,J).GT.0) THEN
            WRITE (MSGTXT, 340) NREJCT(13,J), IF_FREQ(J)/1E9
 340        FORMAT (I6,' REJECTED  as antennas out of the array at  ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 350  CONTINUE

      DO 356 J = 1, MAX_IF
         IF (NREJCT(14,J).GT.0) THEN
            WRITE (MSGTXT, 352) NREJCT(14,J), IF_FREQ(J)/1E9
 352        FORMAT (I6,' REJECTED  by pulsar bin selection at ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
         END IF
 356  CONTINUE

C     Report number of zero filled visibilities and sampler
C     statistics corrected visbilities
      DO 390 J = 1, MAX_IF
         IF (NSSFIX(J).GT.0) THEN
            WRITE (MSGTXT, 360) NSSFIX(J), IF_FREQ(J)/1E9
 360        FORMAT (I6,' CORRECTED for bad sampler statistics at    ',
     *              F8.5, ' GHz')
            CALL ATMSG (MSGTXT)
            WRITE (MSGTXT, 370) SSMIN(J), SSMAX(J), SSSUM(J)/NSSFIX(J)
 370        FORMAT (8X, 'min,max,ave corrections = ',
     *              2(F5.3, ','), F5.3)
            CALL ATMSG (MSGTXT)
         END IF

         IF (NZFILL(J).GT.0) THEN
            WRITE (MSGTXT, 380) NZFILL(J), J
 380        FORMAT (I6,' ZEROED    as there were no data on IF axis',
     *              4X, ' # ', I1)
            CALL ATMSG (MSGTXT)
         END IF

 390  CONTINUE


 999  RETURN
      END


      SUBROUTINE FIXIFL (STR)
      CHARACTER STR*(*)
      INTEGER I, ITRIM, IL

      IL = ITRIM(STR)
      DO 10 I = 1, IL
         IF (STR(I:I).EQ.'-') THEN
            STR(I:I+1) = '  '
         ELSE IF (STR(I:I).EQ.'+') THEN
            STR(I:I) = ' '
         END IF
 10   CONTINUE
C
      RETURN
      END


      SUBROUTINE POLDEC (IF_NSTOK, IF_CSTOK, MSGS, NUMPOL, POLREF,
     *                   POLINC, TRUPOL, IERR)
C-----------------------------------------------------------------------
C     POLDEC decodes polarizations from 2-character strings as stored in
C     the IF table.
C
C     Given:
C          IF_NSTOK
C                        I     Number of polarizations
C          IF_CSTOK(4)
C                        C*2   Polarizations
C          MSGS          I     Tell user what polarizations they have
C
C     Returned:
C          NUMPOL        I     Number of polarizations
C          POLREF        I     Value of first polarization,
C                                 1 -  4 =  I,  Q,  U,  V
C                                -1 - -4 = RR, LL, RL, LR
C                                -5 - -8 = XX, YY, XY, YX
C          POLINC        I     Increment on polarization axis,
C                                +1 (IQUV) or -1 (XX or RR etc)
C          TRUPOL        I     Describes true polarizations in raw data
C                              before conversions and kludging
C                                 1: XX
C                                 2: YY
C                                 3: XX YY
C                                 4: XX YY XY YX
C                                 0: something else
C          IERR          I     Error status,
C                                 0: successful
C                                 1: unrecognized polarization type
C                                 2: invalid polarization order
C                                 3: invalid set of polarizations
C
C     Called:
C          APLSUB: {MSGWRT}
C
C     Algorithm:
C
C     Notes:
C       1) The increment between polarizations must be in monotonic
C          ascending or descending unit steps.
C
C
C-----------------------------------------------------------------------

      INTEGER   MAXPOL
      PARAMETER (MAXPOL = 12)

      INTEGER   IF_NSTOK, I, IERR, IPOLCH(MAXPOL), J, NUMPOL,
     *          POLINC, POLREF, POLS(4), TRUPOL
      LOGICAL MSGS
      CHARACTER POLCH(MAXPOL)*2, IF_CSTOK(4)*2

      INCLUDE 'INCS:DMSG.INC'

      SAVE POLCH
      SAVE IPOLCH

      DATA POLCH  /'I ', 'Q ', 'U ', 'V ', 'RR', 'LL', 'RL', 'LR',
     *             'XX', 'YY', 'XY', 'YX'/
      DATA IPOLCH / 1,  2,  3,  4, -1, -2, -3, -4, -5, -6, -7, -8/
C-----------------------------------------------------------------------
C  Initialize.
      IERR = 0
      TRUPOL = 0
      NUMPOL = IF_NSTOK

C     Tell user what the polarizations really are.
      IF (MSGS) THEN
         WRITE (MSGTXT, 10) (IF_CSTOK(I), I=1,NUMPOL)
 10      FORMAT ('Input polarizations: ',4(A,:,','))
         CALL ATMSG (MSGTXT)
      END IF

C  Apply acceptance tests.  Decode the polarization type and assign
C  numeric code.
      DO 30 I = 1, NUMPOL
         DO 20 J = 1, MAXPOL
            IF (IF_CSTOK(I).EQ.POLCH(J)) THEN
               POLS(I) = IPOLCH(J)
               GO TO 30
            END IF
 20      CONTINUE

         MSGTXT = 'POLDEC: UNRECOGNIZED POLARIZATION TYPE.'
         CALL ATMSG (MSGTXT)
         IERR = 1
         GO TO 999
 30   CONTINUE

C     Check for monotonic unit increments between polarizations.
      POLREF = POLS(1)
      IF (NUMPOL.GT.1) THEN
         POLINC = SIGN(1, POLS(2)-POLS(1))
         DO 40 I = 2, NUMPOL
            IF (POLS(I)-POLS(I-1).NE.POLINC) THEN
               MSGTXT = 'POLDEC: INVALID POLARIZATION SEQUENCE.'
               CALL ATMSG (MSGTXT)
               IERR = 2
               GO TO 999
            END IF
 40      CONTINUE
      END IF

C     Check for allowed sets of polarizations.
      IERR = 3
      IF (NUMPOL.EQ.1) THEN
C        Allow any single polarization, whatever it is.
         IERR = 0
         IF (POLREF.EQ.-5) THEN
            TRUPOL = 1
         ELSE IF (POLREF.EQ.-6) THEN
            TRUPOL = 2
         END IF
      ELSE IF (NUMPOL.EQ.2) THEN
C        Allow (RR,LL), and (XX,YY).
         IF (POLINC.EQ.-1 .AND. POLREF.EQ.-5) THEN
            IERR = 0
            TRUPOL = 3
         END IF
         IF (POLINC.EQ.-1 .AND. POLREF.EQ.-1) IERR = 0
      ELSE IF (NUMPOL.EQ.4) THEN
C        Allow (I,Q,U,V), (RR,LL,RL,LR), and (XX,YY,XY,YX).
         IF (POLINC.EQ.+1 .AND. POLREF.EQ.+1) IERR = 0
         IF (POLINC.EQ.-1 .AND. POLREF.EQ.-1) IERR = 0
         IF (POLINC.EQ.-1 .AND. POLREF.EQ.-5) THEN
            IERR = 0
            TRUPOL = 4
         END IF
      END IF

      IF (IERR.EQ.3) THEN
         MSGTXT = 'POLDEC: INVALID SET OF POLARIZATIONS.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF


 999  RETURN
      END



      SUBROUTINE ATNXCL (NANTS, FREQID, SOUID, IERR)
C-----------------------------------------------------------------------
C     ATNXCL updates the NX (scan index) table and writes dummy CL
C     (calibration) tables entries for the current scan.
C
C     Given:
C          NANTS         I     The number of antennas.
C          FREQID        I     Frequency id number in the FQ table,
C                              corresponds to the FREQSEL random
C                              parameter in the uv data.
C          SOUID         I     Source id number in the SU table,
C                              corresponds to the SOURCE random
C                              parameter in the uv data.
C
C     Given via common ATCTRL:
C          OUTDSK        I     Output disk number.
C          CLSPAN        R     Interval for CL entries, in days.
C
C     Given via common ATUVIO:
C          CNO           I     UV file catalogue slot number.
C
C     Given via common ATSCAN:
C          TSCAN1        D     Scan start time (days).
C          TSCAN2        D     Scan end time (days).
C          VSCAN1        I     Index of the first visibility.
C          VSCAN2        I     Index of the last  visibility.
C          SCANTN(NCA)   L     True for each antenna used in the scan.
C
C     Returned via common MAPHDR:
C          CATBLK(256)   I     Catalog header block.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          ATLOD:  {FIXSRT}
C          APLSUB: {MSGWRT, TABIO}
C          APLNOT: {CALINI, NDXINI, TABCAL, TABNDX}
C
C     Algorithm:
C
C     Notes:
C       1) On error, the NX and CL files will be deleted and the UV file
C          marked as unsorted.
C
C-----------------------------------------------------------------------

      INCLUDE 'INCS:PUVD.INC'

      INTEGER   MAXIF2
      PARAMETER (MAXIF2 = 2*MAXIF)

      INTEGER   CLBUFF(512), CLKOLS(MAXCLC), CLLUN, CLNUMV(MAXCLC),
     *          CLVER, FREQID, IANT, ICLRNO, SOUID, IENT, IERR, INXRNO,
     *          JERR, NANTS, NENT, NUMANT, NXBUFF(512),
     *          NXKOLS(MAXNXC), NXLUN, NXNUMV(MAXNXC), NXVER,
     *          REFA(2,MAXIF), SUBARR, NPOL, NTERMS
      REAL      CIMAG(2,MAXIF), CREAL(2,MAXIF), DELAY(2,MAXIF),
     *          DOPOFF(MAXIF), DTIME, GMMOD, IFR, NXTIME,
     *          SOLNWT(2,MAXIF), RATE(2,MAXIF), ATMOS, DATMOS,
     *          MBDELY(2), CLOCK(2), DCLOCK(2), DISP(2), DDISP(2)
      DOUBLE PRECISION GEODLY(3), CLTIME, TDIFF

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'

      DATA IFR, GEODLY /0.0, 3*0D0/
      DATA DOPOFF /MAXIF*0.0/
      DATA ATMOS, DATMOS /0.0, 0.0/
      DATA MBDELY, CLOCK, DCLOCK /2*0.0, 2*0.0, 2*0.0/
      DATA DISP, DDISP /2*0.0, 2*0.0/
      DATA CREAL, CIMAG  /MAXIF2*1.0, MAXIF2*0.0/
      DATA DELAY, RATE   /MAXIF2*0.0, MAXIF2*0.0/
      DATA SOLNWT /MAXIF2*1.0/
      DATA REFA   /MAXIF2*0/
C-----------------------------------------------------------------------
C  Update the NX file.
      NXVER = 1
      NXLUN = 43
      CALL NDXINI ('WRIT', NXBUFF, OUTDSK, CNO, NXVER, CATBLK, NXLUN,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATNX: ERROR OPENING THE NX (SCAN INDEX) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Check start and stop visibility indices.
      IF (VSCAN1.LE.0 .OR. VSCAN2.LE.0 .OR. VSCAN2.LT.VSCAN1) THEN
         WRITE (MSGTXT, 10) VSCAN1, VSCAN2
 10      FORMAT ('ATNX: BAD VISIBILITY INDICES, START',I5,', END',I5)
         CALL ATMSG (MSGTXT)
         IERR = 1
         GO TO 999
      END IF

C     Make an entry for this scan.
      DTIME  = TSCAN2 - TSCAN1
      NXTIME = TSCAN1 + DTIME/2.0
      SUBARR = 1
      CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *   DTIME, SOUID, SUBARR, VSCAN1, VSCAN2, FREQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 20) IERR
 20      FORMAT ('ATNX: TABIO ERROR',I3,' WRITING NX TABLE.')
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Close the NX table.
      CALL TABIO ('CLOS', 0, 0, NXBUFF, NXBUFF, JERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATNX: ERROR CLOSING NX (SCAN INDEX) TABLE.'
         CALL ATMSG (MSGTXT)
      END IF


C  Update the CL file.
      CLVER  = 1
      CLLUN  = 44
      NUMANT = NANTS
      IF (CATD(KDCRV+1).GT.0.5) THEN
C        One solution set for IQUV.
         NPOL = 1
      ELSE
C        Two solution sets for XX,YY,XY,YX or pseudo RR,LL,RL,LR.
         NPOL = MIN(2, CATBLK(KINAX+1))
      END IF
      NTERMS = 1
      GMMOD  = 1.0
      CALL CALINI ('WRIT', CLBUFF, OUTDSK, CNO, CLVER, CATBLK, CLLUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NPOL, NUMIF, NTERMS,
     *   GMMOD, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATNXCL: ERROR OPENING THE CL (CALIBRATION) TABLE.'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Work out number of CL entries in this (AIPS) scan
      NENT = INT((TSCAN2-TSCAN1)/CLSPAN) + 1
      TDIFF = TSCAN2 - (NENT-1)*CLSPAN - TSCAN1
      IF (TDIFF .GT. 1.0D0/86400.0D0) NENT = NENT + 1

C     Write CL table
      CLTIME = TSCAN1
      SUBARR = 1
      DO 50 IENT = 1, NENT
         DO 40 IANT = 1, NANTS
            IF (.NOT.SCANTN(IANT)) GO TO 40
            CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *         NPOL, NUMIF, CLTIME, CLSPAN, SOUID, IANT, SUBARR,
     *         FREQID, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *         CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *         SOLNWT, REFA, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 30) IERR
 30            FORMAT ('ATNXCL: ERROR',I3,' WRITING CL ENTRY')
               CALL ATMSG (MSGTXT)
               GO TO 999
            END IF
 40      CONTINUE

         CLTIME = MIN(CLTIME+CLSPAN, DBLE(TSCAN2))
 50   CONTINUE

C     Close the CL table.
      CALL TABIO ('CLOS', 0, 0, CLBUFF, CLBUFF, JERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ATNXCL: ERROR CLOSING CL (CALIBRATION) TABLE.'
         CALL ATMSG (MSGTXT)
      END IF


C  Error exit.
 999  IF (IERR.NE.0) THEN
         CALL FIXSRT (JERR)
      END IF

      RETURN
      END




      SUBROUTINE TIMCON (TIME, DHMS)
C-----------------------------------------------------------------------
C     TIMCON converts time in days to integer DD,HH,MM,SS.
C
C     Given:
C          TIME          D     Time in days.
C
C     Returned:
C          DHMS(4)       I     Time as DD HH MM SS.
C
C     Called:
C          none
C
C
C-----------------------------------------------------------------------
      INTEGER   DHMS(4)
      DOUBLE PRECISION FRAC, TIME
C-----------------------------------------------------------------------
      FRAC = TIME
      DHMS(1) = FRAC
      FRAC = (FRAC - REAL(DHMS(1))) * 24.0
      DHMS(2) = FRAC

      FRAC = (FRAC - REAL(DHMS(2))) * 60.0
      DHMS(3) = FRAC

      FRAC = (FRAC - REAL(DHMS(3))) * 60.0
      DHMS(4) = NINT(FRAC)

      RETURN
      END


      SUBROUTINE SYSCIN (LOCDOT, DHMS, SYSNUM, BADSS,
     *                   NPHCLP, NTXCLP, NTYCLP, MAXREJ, NREJCT, IERR)
C-----------------------------------------------------------------------
C     SYSCIN processes a syscal record.
C
C     Parameters set by ATLOD.INC:
C          NCA, NSYSCP, MAXSFQ, MAXSSU, MAXPST, MAXTST
C
C     Parameters set by RPFITS.INC:
C          ANT_MAX, MAX_IF, MAX_SU
C
C     Given:
C          LOCDOT        I     -1  => Undo on-line correction
C                               0  => Do nothing
C                               N  => Undo on-line Tsys and redo
C                                     with N averaged Tsys measurements
C                              Will be -1 or > 1 to be in this routine
C          DHMS(4)       I     UT of the scan
C
C     Given via common ATCTRL:
C          DOSTOK        L     If true convert linear polarizations to
C                              Stokes.
C          NSTAKP        I     Number of integration periods to average
C                              XY phases over for Stokes conversion.
C          DOXYFL        L     If true drop visibilties when XY
C                              phases bad.
C          PRTSYS        I     1: List system cal info
C                              2: Warn about accumulator resets
C                              3: 1 & 2
C          SSCHK         I     1: list ants. with bad sampler statistics
C                              2: Flag vis. which have bad sampler stats
C                              3: 1 + 2
C                              4: Correct data when samplers bad
C          TRUPOL        I     TRue polarizations
C                              1 = XX, 2 = YY, 3 = XX,YY 4 = XX,YY,XY,YX
C
C     Given via common ATSCAN:
C          ANTIN()       L     True if antenna in array. e.g., if
C                              ANTIN(3) = .FALSE. then antenna 3 is
C                              not in the array.
C
C     Given via common VISBUF:
C          SRCNUM        I     RPFITS source random parameter.
C
C     Given via common ATSYSC:
C          JULSYS        D     Julian date when Tsys correction can
C                              be made offline.
C          SSTOL         R     Tolerance to accept samplers
C
C     Given via RPFITS common ANTEN:
C          NANT          I     The number of antennas.
C
C     Given via RPFITS common SC:
C          SC_Q          I     Number of syscal parameters per IF per
C                              antenna.
C          SC_IF         I     Number of IFs per antenna.
C          SC_ANT        I     Number of antennas.
C          SC_CAL(SC_Q,SC_IF,SC_ANT)
C                        R     Syscal parameters.
C
C     Given and returned via common ATESEL:
C          FQTAGS(MAX_IF)
C                        I     Translation from frequency identification
C                              number in the RPFITS header to that of
C                              the FQ file.
C          SUTAGS(MAX_SU,MAX_IF)
C                        I     Translation from source identification
C                              number in the RPFITS header to that of
C                              the SU file.
C                              for this combination of antenna and FQ
C
C     Given and returned:
C          NPHCLP(NCA,MAX_IF)
C                        I     Number of phase differences clipped.
C          NTXCLP(NCA,MAX_IF)
C                        I     Number of X Tsys points clipped.
C          NTYCLP(NCA,MAX_IF)
C                        I     Number of Y Tsys points clipped.
C          NREJCT(2,MAX_IF)    NUmber of SYSCAL groups processed
C                              this scan per IF
C     Given and returned via common ATSYSC:
C          NSYSPH(NCA,MAXSSF,MAXSFQ)
C                        I     Number of XY phase accumulations for each
C                              antenna in time series so far.  May be
C                              reset to 1 here.
C          NSYSTX(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        I     Number of X "temperatures" accumulated
C                              for ANT, FREQID, IF & SOURCE so far.
C          NSYSTY(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        I     Number of Y "temperatures" accumulated
C                              for ANT, FREQID, IF & SOURCE so far.
C          PHSTAK(MAXPST,NCA,MAXSSF,MAXSFQ)
C                        R     Accumulated stacks of XY phases.
C          PHSAVE(NCA,MAXSSF,MAXSFQ)
C                        R     Average of XY phase stacks.  Phases are
C                              accumulaed for each antenna frequency,
C                              and IF.
C          TXSTAK(MAXTST,NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        R     Accumulated stacks of X Tsys values.
C          TXAVE(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        R     Average of X Tsys stacks.
C          TYSTAK(MAXTST,NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        R     Accumulated stacks of Y Tsys values.
C          TYAVE(NCA,MAXSSF,MAXSFQ,MAXSSU)
C                        R     Average of Y Tsys stacks.
C                              System temperatures are accumulated for
C                              each antenna, frequency, IF, and source.
C          PHSCLP        R     Phase clipping tolerence.
C
C     Returned via common ATCTRL
C          CLIPPD(NCA,MAXSSF,MAXSFQ)
C                        L     True if antenna XY phase is clipped, else
C                              false.
C
C     Returned:
C          SYSNUM(ANT_MAX,MAX_IF,NSYSCP)
C                        R     System calibration parameters described
C                              in the prologue to SYSEXT.
C          BADSS(ANT_MAX,MAX_IF)
C                        L     True if antenna has bad sampler stats
C                              in either X or Y
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          ATLOD:  {ATMSG, ATSUAD, DRIFT, SYSEXT, SYSFLG}
C          APLSUB: {IROUND, MSGWRT}
C
C     Algorithm:
C          It is assumed that the noise in XY phase, Tx, and Ty is
C          roughly constant from antenna to antenna.  This is a
C          reasonable, although not excellent assumption.
C
C          At least MINSTK points must have been accumulated in the
C          stack before any drift analysis is attempted.  Drifts are
C          detected by comparing the current datum for each antenna with
C          the mean of the current stack.  If the mean of the difference
C          (summed over all antennas) has changed by more than TOL times
C          the mean stack width (i.e. a measure of the noise), then all
C          accumulators are reset for all antennas.  Points for a given
C          antenna that are severely discrepant from their local mean
C          are not included in the averaged mean on the assumption that
C          they are bad.
C
C          Either the accumulators are reset or they aren't, so in
C          either case no severe damage is done.  The worst is that a
C          drift has occurred, but wasn't caught, so the stack average
C          and stack are initially not appropriate.
C
C     Notes:
C       1) The IF numbers stored in SC_CAL(SC_Q,SC_IF,SC_ANT) are
C          common to all antennas.  In other words, the SC_CAL(2,IIF,*)
C          are all identical.
C
C       2) If the XY phase has jumped since the last syscal record (it
C          shouldn't have) or Tsys has drifted appreciably, the
C          corresponding stacks will be reinitialized, otherwise the
C          current stack and stack average would reflect incorrect
C          values.
C
C       3)   When asking to correct data for bad samplers, the data
C          are correctable only if
C
C          abs(pos-17.1) < TOL,  abs(neg-17.1) < TOL
C             where TOL defaults to 12, but the user has control of it
C          abs(zero-50) < 5
C          abs(pos-neg) < 5
C
C          Data are discarded if the samplers are outside of
C          these ranges
C
C-----------------------------------------------------------------------

C     Set clipping tolerances, STACK and STREAM, for phase, Tx and Ty.
      REAL TPHSTK, TPHSTR, TTXSTK, TTXSTR, TTYSTK, TTYSTR
      PARAMETER (TPHSTK =  5.0, TPHSTR = 40.0,
     *           TTXSTK = 20.0, TTXSTR = 15.0,
     *           TTYSTK = 20.0, TTYSTR = 15.0)

C     Put stack mean equal to true mean or current datum.  Only
C     affects stack average when accumulating first NSTACK points.
      LOGICAL   DOPSTM, DOTSTM
      PARAMETER (DOPSTM = .TRUE., DOTSTM = .TRUE.)

C     Set minimum stack size and tolerance for phase drift checking.
      INTEGER   MINSTK
      REAL      PHTOL, TSTOL
      PARAMETER (PHTOL = 5.0, TSTOL = 3.0, MINSTK = 3)

      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'

      LOGICAL   BAD(NCA), CONST(NCA), DUMMY(NCA), WORKP(MAXPST),
     *          WORKT(MAXTST), BADSS(ANT_MAX,MAX_IF), GOODSS
      INTEGER   FREQID, IANT, SOUID, IERR, IIF,
     *          ITOT, DHMS(4), LOCDOT,
     *          JFNUM(ANT_MAX,MAX_IF), IFAXID, KFNUM,
     *          NPHCLP(ANT_MAX,MAX_IF), NTXCLP(ANT_MAX,MAX_IF),
     *          NTYCLP(ANT_MAX,MAX_IF), MAXREJ, NREJCT(MAXREJ,MAX_IF)
      REAL      AVEDIF, AVEWID, SUMDIF, SUMWID, TCLP, XYPHS,
     *          SYSNUM(ANT_MAX,MAX_IF,NSYSCP)

C     Sampler tolerances and means
      REAL TOLZ, TOLPN
      PARAMETER (TOLZ = 5.0, TOLPN = 5.0)
C-----------------------------------------------------------------------

C     Initialize IF number array and sampler quality array
      DO 7 IIF = 1, SC_IF
         DO 5 IANT = 1, NANT
            JFNUM(IANT,IIF) = 0
            BADSS(IANT,IIF) = .FALSE.
 5       CONTINUE
 7    CONTINUE

C     Copy system calibration information into a more manageable array.
      CALL SYSEXT (SC_Q, SC_IF, SC_ANT, SC_CAL, NSYSCP, MAX_IF,
     *             ANT_MAX, SYSNUM, JFNUM)

C     Loop for each IF number in the syscal array.
      DO 170 IIF = 1, SC_IF

C        Assume IF number same for all antennas; zeroes checked
C        in SYSEXT
         KFNUM = JFNUM(1,IIF)

C        Number of SYSCALs per IF processed this scan
         NREJCT(2,KFNUM) = NREJCT(2,KFNUM) + 1

C        Check legitimacy of KFNUM.
         IF (KFNUM.LT.1 .OR. KFNUM.GT.MAX_IF) GO TO 170

C        Get the FQ id. and location on the AIPS IF axis
         IF (FQTAGS(KFNUM).EQ.-1) THEN
C           Not wanted, ignore it.
            GO TO 170
         ELSE
            FREQID = FQTAGS(KFNUM)
            IFAXID = SELAX(KFNUM)
         END IF

         IF (FREQID.GT.MAXSFQ) THEN
            MSGTXT = 'SYSCIN: TOO MANY FREQIDs FOR SYSTEM ' //
     *               'CALIBRATION ACCUMULATION.'
            CALL ATMSG (MSGTXT)
            IERR = 1
            GO TO 999
         END IF

C        Get the SU id.
         IF (SUTAGS(SRCNUM,KFNUM).EQ.-1) THEN
C           Not wanted, ignore it.
            GO TO 170
         ELSE
            SOUID = SUTAGS(SRCNUM,KFNUM)
         END IF


C        Assess quality of sampler statistics.  Ignore SYSCAL flag
C        status as those things are irrelevant to the samplers
         IF (SSCHK.GE.1 .AND. SSCHK.LE.4) THEN

C           Loop over all antennas
            DO 30 IANT = 1, NANT

C              Should really use an ANTNUM (like KFNUM) rather
C              than IANT, but in practice they are the same.
               IF (ANTIN(IANT)) THEN

C                 Check the sampler stats as directed
                  IF (SSCHK.LE.3) THEN

C                    Options to warn user and/or drop data with
C                    bad sampler stats; initialize
                     GOODSS = .TRUE.

C                    Check the stats for the polarizations present
                     IF (TRUPOL.EQ.1 .OR. TRUPOL.EQ.3 .OR.
     *                   TRUPOL.EQ.4) THEN

C                       XX required
                        IF (ABS(SYSNUM(IANT,KFNUM,8)-SSEXP).GT.SSTOL
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,10)-SSEXP).GT.SSTOL)
     *                     GOODSS = .FALSE.
                     END IF
C
                     IF (GOODSS .AND. (TRUPOL.EQ.2 .OR. TRUPOL.EQ.3
     *                           .OR.  TRUPOL.EQ.4)) THEN

C                       YY required
                        IF (ABS(SYSNUM(IANT,KFNUM,11)-SSEXP).GT.SSTOL
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,13)-SSEXP).GT.SSTOL)
     *                     GOODSS = .FALSE.
                     END IF
                     BADSS(IANT,KFNUM) = .NOT.GOODSS

                  ELSE
C                    Option to correct data for bad sampler stats
C                    Additional checks to make sure that the
C                    pos and neg levels are similar and that the
C                    zero level is not too far wrong; initialize
                     GOODSS = .TRUE.

C                    Check the stats for the polarizations present
                     IF (TRUPOL.EQ.1 .OR. TRUPOL.EQ.3 .OR.
     *                   TRUPOL.EQ.4) THEN

C                       XX required
                        IF (ABS(SYSNUM(IANT,KFNUM, 8)-SSEXP).GT.SSTOL
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,10)-SSEXP).GT.SSTOL
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,8) -
     *                          SYSNUM(IANT,KFNUM,10)).GT.TOLPN
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM, 9)- 50.0).GT.TOLZ)
     *                     GOODSS = .FALSE.
                     END IF

C
                     IF (GOODSS .AND. (TRUPOL.EQ.2 .OR. TRUPOL.EQ.3
     *                           .OR.  TRUPOL.EQ.4)) THEN

C                       YY required
                        IF (ABS(SYSNUM(IANT,KFNUM,11)-SSEXP).GT.SSTOL
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,13)-SSEXP).GT.SSTOL
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,11) -
     *                          SYSNUM(IANT,KFNUM,13)).GT.TOLPN
     *                      .OR.
     *                      ABS(SYSNUM(IANT,KFNUM,12)- 50.0).GT.TOLZ)
     *                     GOODSS = .FALSE.
                     END IF
C
                     BADSS(IANT,KFNUM) = .NOT.GOODSS
                  END IF
               END IF
 30         CONTINUE
         END IF


         IF (DOSTOK .AND. SOUID.GT.MAXSSU) THEN
            MSGTXT = 'SYSCIN: TOO MANY SOURCES FOR SYSTEM ' //
     *               'CALIBRATION ACCUMULATION.'
            CALL ATMSG (MSGTXT)
            IERR = 1
            GO TO 999
         END IF

C        Process XY-phase differences
         IF (DOSTOK .OR. DOXYFL .OR. YGNROT) THEN
            IF (USERXY) THEN
C
C              Use XYPHASE values as mean XY phase value with
C              which to test bad XY phases on.
               DO 40 IANT = 1, NANT
                  XYPHS = XYPHAS(IANT,XYPHPT(IFAXID,FREQID))
                  IF (ABS(SYSNUM(IANT,KFNUM,1)-XYPHS).GT.PHSCLP) THEN
                     CLIPPD(IANT,IFAXID,FREQID) = .TRUE.
                  ELSE
                     CLIPPD(IANT,IFAXID,FREQID) = .FALSE.
                  END IF
 40            CONTINUE
            ELSE
C
C              Do complicated analaysis on the on-line XY
C              phases, clipping bad ones.
C
               IF (NSTAKP.GE.MINSTK) THEN
C              Restart accumulations if XY phase diff has jumped.
                  DO 50 IANT = 1, NANT
                     BAD(IANT) = .FALSE.
 50               CONTINUE

C                 Accumulate statistics for phase drift analysis.
                  ITOT = 0
                  SUMDIF = 0.0
                  SUMWID = 0.0
                  CALL DRIFT (NANT, NSTAKP, MAXPST, NCA,
     *               NSYSPH(1,IFAXID,FREQID), BAD, MINSTK, TPHSTR,
     *               PHSTAK(1,1,IFAXID,FREQID), SYSNUM(1,KFNUM,1),
     *               ANTIN, SYSNUM(1,KFNUM,15), ITOT, SUMDIF, SUMWID)

                  IF (ITOT.GT.0) THEN
C                    Find mean difference and width.
                     AVEDIF = SUMDIF / ITOT
                     AVEWID = SUMWID / ITOT

C                    Reset accumulators if necessary.
                     IF (AVEDIF.GT.PHTOL*AVEWID) THEN
                        DO 60 IANT = 1, NANT
                           NSYSPH(IANT,IFAXID,FREQID) = 0
 60                     CONTINUE

                        IF (PRTSYS.EQ.2 .OR. PRTSYS.EQ.3) THEN
C                          Inform user
                           WRITE (MSGTXT, 70) SOUID, FREQID, IFAXID,DHMS
 70                        FORMAT ('XYphs ave reset: srcID', I3,
     *                             ' FqID ', I2, ' IFaxID ', I2,
     *                             ' at UT ', I3, I3.2, ':', I2.2,
     *                             ':', I2.2)
                           CALL ATMSG (MSGTXT)

                           WRITE (MSGTXT, 80) AVEDIF, AVEWID,
     *                     AVEDIF/(PHTOL*AVEWID)
 80                        FORMAT ('Diff, width, diff/tol*width = ',
     *                        3(F7.2,2X))
                           CALL ATMSG (MSGTXT)
                        END IF
                     END IF
                  END IF
               END IF

C              Edit antenna XY phase differences to remove bad points.
               CALL SYSFLG (ANTIN, DOPSTM, TPHSTR, TPHSTK, PHSCLP,
     *            NSYSPH(1,IFAXID,FREQID), NSTAKP, NANT, MAXPST, NCA,
     *            PHSTAK(1,1,IFAXID,FREQID), SYSNUM(1,KFNUM,1),
     *            SYSNUM(1,KFNUM,15), PHSAVE(1,IFAXID,FREQID), WORKP,
     *            CONST, NPHCLP(1,KFNUM), CLIPPD(1,IFAXID,FREQID))

C              If stack constant, start accumulating points into the
C              stack without clipping again.  Begin with what we
C              already have.
               DO 90 IANT = 1, NANT
                  IF (CONST(IANT)) NSYSPH(IANT,IFAXID,FREQID) = 1
 90            CONTINUE
            END IF
         END IF


C        Process X,Y system temperatures.
         IF (LOCDOT.EQ.-1) THEN

C           We are going to undo the on-line correction, so just
C           indicate there is a value (be it flagged or unflagged,
C           it must still be undone)
            DO 100 IANT = 1, NANT
               NSYSTX(IANT,IFAXID,FREQID,SOUID) = 1
               NSYSTY(IANT,IFAXID,FREQID,SOUID) = 1
 100        CONTINUE
         ELSE IF (LOCDOT.GT.1) THEN

            IF (LOCDOT.GE.MINSTK) THEN

C              Restart accumulations if Tsys has drifted;
C              accumulate statistics for Tx drift analysis.

               DO 110 IANT = 1, NANT
                  BAD(IANT) = .FALSE.
 110           CONTINUE
               ITOT = 0
               SUMDIF = 0.0
               SUMWID = 0.0
               CALL DRIFT (NANT, LOCDOT, MAXTST, NCA,
     *            NSYSTX(1,IFAXID,FREQID,SOUID), BAD, MINSTK, TTXSTR,
     *            TXSTAK(1,1,IFAXID,FREQID,SOUID), SYSNUM(1,KFNUM,2),
     *            ANTIN, SYSNUM(1,KFNUM,15), ITOT, SUMDIF, SUMWID)

C              Continue to accumulate statistics with Ty drift analysis.
               CALL DRIFT (NANT, LOCDOT, MAXTST, NCA,
     *            NSYSTY(1,IFAXID,FREQID,SOUID), BAD, MINSTK, TTYSTR,
     *            TYSTAK(1,1,IFAXID,FREQID,SOUID), SYSNUM(1,KFNUM,3),
     *            ANTIN, SYSNUM(1,KFNUM,15), ITOT, SUMDIF, SUMWID)

               IF (ITOT.GT.0) THEN
C                 Find mean difference and width.
                  AVEDIF = SUMDIF / ITOT
                  AVEWID = SUMWID / ITOT

C                 Reset accumulators if necessary.
                  IF (AVEDIF.GT.TSTOL*AVEWID) THEN
                     DO 120 IANT = 1, NANT
                        NSYSTX(IANT,IFAXID,FREQID,SOUID) = 0
                        NSYSTY(IANT,IFAXID,FREQID,SOUID) = 0
 120                 CONTINUE

                     IF (PRTSYS.EQ.2 .OR. PRTSYS.EQ.3) THEN
C                       Inform user.
                        WRITE (MSGTXT, 130) SOUID, FREQID, IFAXID, DHMS
 130                    FORMAT ('Tsys reset for srcID', I3,
     *                     ' FqID ', I2, ' IFaxID ', I2, ' at UT ',
     *                     I3, I3.2, ':', I2.2, ':', I2.2)
                        CALL ATMSG (MSGTXT)

                        WRITE (MSGTXT, 140) AVEDIF, AVEWID,
     *                     AVEDIF/(TSTOL*AVEWID)
 140                    FORMAT ('Diff, width, diff/tol*width = ',
     *                     3(F7.2,2x))
                        CALL ATMSG (MSGTXT)
                     END IF
                  END IF
               END IF
            END IF


C           Edit X system temperatures to remove bad points. Replace
C           bad points by mean only when outside clipping criterion
C           and at least 1 K (sqrt(10*Tsys)=3.2) from mean.

            TCLP = 3.2
            CALL SYSFLG (ANTIN, DOTSTM, TTXSTR, TTXSTK, TCLP,
     *         NSYSTX(1,IFAXID,FREQID,SOUID), LOCDOT, NANT, MAXTST,
     *         NCA, TXSTAK(1,1,IFAXID,FREQID,SOUID), SYSNUM(1,KFNUM,2),
     *         SYSNUM(1,KFNUM,15), TXAVE(1,IFAXID,FREQID,SOUID), WORKT,
     *         CONST, NTXCLP(1,KFNUM), DUMMY)

C           If stack constant start accumulating into the stack
C           without clipping again.  STart with what we have.
            DO 150 IANT = 1, NANT
               IF (CONST(IANT)) NSYSTX(IANT,IFAXID,FREQID,SOUID) = 1
 150        CONTINUE

C           Edit Y system temperatures to remove bad points.

            TCLP = 3.2
            CALL SYSFLG (ANTIN, DOTSTM, TTYSTR, TTYSTK, TCLP,
     *         NSYSTY(1,IFAXID,FREQID,SOUID), LOCDOT, NANT, MAXTST,
     *         NCA, TYSTAK(1,1,IFAXID,FREQID,SOUID), SYSNUM(1,KFNUM,3),
     *         SYSNUM(1,KFNUM,15), TYAVE(1,IFAXID,FREQID,SOUID), WORKT,
     *         CONST, NTYCLP(1,KFNUM), DUMMY)

C           If stack constant start accumulating into the stack
C           without clipping again.  STart with what we have.
            DO 160 IANT = 1, NANT
               IF (CONST(IANT)) NSYSTY(IANT,IFAXID,FREQID,SOUID) = 1
 160        CONTINUE
         END IF
C
 170  CONTINUE

C      write(88,*) (txave(iant,1,1,1), iant=1,6)
C      write(89,*) (tyave(iant,1,1,1), iant=1,6)



 999  RETURN
      END



      SUBROUTINE SHADIN (IERR)
C-----------------------------------------------------------------------
C     SHADIN determines if any of the antennas are shadowed in the
C     current integration.
C
C     Given via parameters in ATLOD.INC:
C          NCA           I     Number of Compact Array antennas.
C
C     Given via common VISBUF:
C          U,V,W         I     Visibility coordinates, in meters.
C          BASLEN(,)     R     Compact array baselines, in meters.
C          BASMIN        R     Minimum baseline length, in meters.
C
C     Returned via common VISBUF:
C          SHADED(NCA)   L     True if the antenna is shadowed.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          none
C
C     Algorithm:
C          All baselines must be tested before a shadowed antenna can be
C          discovered.  Any baseline involving that antenna can then be
C          rejected.
C
C          The shadowing test is easy to perform if the (u,v,w) baseline
C          coordinates are known, (shadowing if SQRT(U*U+V*V) < SHADOW).
C          However, this would require that ATUV store all 15 baselines
C          associated with an integration, using as much as 1.5 Mbytes
C          for 8192 spectral channels, too much to take for granted on a
C          workstation.
C
C          However, the (x,y,z) coordinates of each antenna are
C          available via the AN table, although the (x,y,z) coordinates
C          of the source are not readily available.  These (x,y,z)
C          coordinates are used by ATANT to calculate the baseline
C          lengths.
C
C          Therefore, given the (u,v,w) coordinates of the first
C          visibility, the component of the baseline unit vector
C          perpendicular to the unit vector in the direction of the
C          source may be computed.  Since all baselines in the Compact
C          Array are co-linear, the projected (u,v) separation of the
C          antennas can then be calculated from the baseline lengths.
C
C          Which of the two antennas in a baseline is shadowed can be
C          deduced from the sign of the w coordinate (which must be the
C          same for all co-linear baselines).
C
C     Notes:
C       1) Although symmetric, BASLEN is set up as a matrix for ease
C          of access.  Also, the smallest baseline is provided and
C          tested first for speed.
C
C
C-----------------------------------------------------------------------

      INTEGER   IANT, IERR, JANT
      REAL      BPS, UVWSQ

      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
      IERR = 1

C     Reset the array.
      DO 10 IANT = 1, NCA
         SHADED(IANT) = .FALSE.
 10   CONTINUE

C     Compute the component of the baseline unit vector perpendicular to
C     the source unit vector.
      UVWSQ = U*U + V*V + W*W
      IF (UVWSQ.EQ.0.0) GO TO 999
      BPS = SQRT(1.0 - W*W/UVWSQ)

C     Test the shortest baseline first.
      IF (BASMIN*BPS.LT.SHADOW) THEN
C        Test all baselines.
         DO 30 IANT = 1, NCA
            DO 20 JANT = IANT+1, NCA
               IF (BASLEN(IANT,JANT)*BPS.LT.SHADOW) THEN
                  IF (W.LT.0) THEN
                     SHADED(IANT) = .TRUE.
                  ELSE
                     SHADED(JANT) = .TRUE.
                  END IF
               END IF
 20         CONTINUE
 30      CONTINUE
      END IF
      IERR = 0


 999  RETURN
      END



      SUBROUTINE SSCOR (XNEGA1, XZERA1, XPOSA1, YNEGA1, YZERA1, YPOSA1,
     *                  XNEGA2, XZERA2, XPOSA2, YNEGA2, YZERA2, YPOSA2,
     *                  TRUPOL, SSEXP, FACXX, FACYY, FACXY, FACYX,
     *                  SSFIX, SSFAC, SSMIN, SSMAX)
C-----------------------------------------------------------------------
C     Find factors to correct for bad sampler statistics
C
C  Input:
C    XNEG,XZER,XPOS   R     Sampler stats for X feed for each antenna
C                           Ideally they should be 17.1,50.0,17.1 %
C    YNEG,YZER,YPOS   R     Sampler stats for Y feed for each antenna
C    TRUPOL           I     Describes true polarizations in raw data
C                           1: XX
C                           2: YY
C                           3: XX YY
C                           4: XX YY XY YX
C    SSEXP            R     Expected value for neg and pos samplers.
C                           Either 17.1 (older data) or 17.3% (newer)
C  Input/output
C    FAC*             R     Factors to apply to visibilities
C    SSFIX            L     True if at least one correlation
C                           in the visibility needs correcting
C  Output:
C    SSFAC            R     Average correction factor from all
C                           correlations that were fixed up
C    SSMIN,MAX        R     Min and max corrections
C
C  Notes:
C     Work out the correction factors even if the syscal group
C     is flagged.   The total power in the receivers, which
C     sets the sampler levels, isn't affected by the things that
C     cause the syscal group to be flagged (not on source or
C     delay unit time out etc).
C
C     If the neg and pos stats are wrong by more than 0.2% we
C     make the correction.  This is 1% in gain.  Otherwise the
C     correction is negligible.
C
C     Data for which the samplers are too far gone have already
C     been discarded.  These checks are made in SYSCIN
C
C-----------------------------------------------------------------------
      INTEGER TRUPOL
      REAL XNEGA1, XZERA1, XPOSA1, YNEGA1, YZERA1, YPOSA1, XNEGA2,
     *XZERA2, XPOSA2, YNEGA2, YZERA2, YPOSA2, FACXX, FACYY, FACXY,
     *FACYX, FAC, TWOBIT_GAIN_ADJUST, SSFAC, SSMAX, SSMIN, SSEXP
      LOGICAL SSFIX
C
      REAL TOL
      PARAMETER (TOL = 0.2)
      INTEGER NAVE
C-----------------------------------------------------------------------
      SSMAX = -1.0
      SSMIN = 10.0
C
      SSFIX = .FALSE.
      SSFAC = 0.0
      NAVE = 0
C
      IF (TRUPOL.NE.2) THEN

C        Need XX
         IF (ABS(XNEGA1-SSEXP).GT.TOL.OR.ABS(XPOSA1-SSEXP).GT.TOL.OR.
     *       ABS(XNEGA2-SSEXP).GT.TOL.OR.ABS(XPOSA2-SSEXP).GT.TOL) THEN
            FAC = TWOBIT_GAIN_ADJUST (SSEXP, XNEGA1, XZERA1, XPOSA1,
     *                                XNEGA2, XZERA2, XPOSA2)
            SSMAX = MAX(SSMAX, FAC)
            SSMIN = MIN(SSMIN, FAC)
            SSFIX = .TRUE.
            SSFAC = SSFAC + FAC
            NAVE = NAVE + 1
         ELSE
            FAC = 1.0
         END IF
         FACXX = FACXX * FAC
      END IF

      IF (TRUPOL.NE.1) THEN

C        Need YY
         IF (ABS(YNEGA1-17.1).GT.TOL.OR.ABS(YPOSA1-17.1).GT.TOL.OR.
     *       ABS(YNEGA2-17.1).GT.TOL.OR.ABS(YPOSA2-17.1).GT.TOL) THEN
            FAC = TWOBIT_GAIN_ADJUST (SSEXP, YNEGA1, YZERA1, YPOSA1,
     *                                YNEGA2, YZERA2, YPOSA2)
            SSMAX = MAX(SSMAX, FAC)
            SSMIN = MIN(SSMIN, FAC)
            SSFIX = .TRUE.
            SSFAC = SSFAC + FAC
            NAVE = NAVE + 1
         ELSE
            FAC = 1.0
         END IF
         FACYY = FACYY * FAC
      END IF


      IF (TRUPOL.EQ.4) THEN

C        Need XY and YX
         IF (ABS(XNEGA1-SSEXP).GT.TOL.OR.ABS(XPOSA1-SSEXP).GT.TOL.OR.
     *       ABS(YNEGA2-SSEXP).GT.TOL.OR.ABS(YPOSA2-SSEXP).GT.TOL) THEN
            FAC = TWOBIT_GAIN_ADJUST (SSEXP, XNEGA1, XZERA1, XPOSA1,
     *                                YNEGA2, YZERA2, YPOSA2)
            SSMAX = MAX(SSMAX, FAC)
            SSMIN = MIN(SSMIN, FAC)
            SSFIX = .TRUE.
            SSFAC = SSFAC + FAC
            NAVE = NAVE + 1
         ELSE
            FAC = 1.0
         END IF
         FACXY = FACXY * FAC
C
         IF (ABS(YNEGA1-SSEXP).GT.TOL.OR.ABS(YPOSA1-SSEXP).GT.TOL.OR.
     *       ABS(XNEGA2-SSEXP).GT.TOL.OR.ABS(XPOSA2-SSEXP).GT.TOL) THEN
            FAC = TWOBIT_GAIN_ADJUST (SSEXP, YNEGA1, YZERA1, YPOSA1,
     *                                XNEGA2, XZERA2, XPOSA2)
            SSMAX = MAX(SSMAX, FAC)
            SSMIN = MIN(SSMIN, FAC)
            SSFIX = .TRUE.
            SSFAC = SSFAC + FAC
            NAVE = NAVE + 1
         ELSE
            FAC = 1.0
         END IF
         FACYX = FACYX * FAC
      END IF

C     Work out average correction factor
      IF (NAVE.GT.0) SSFAC = SSFAC / NAVE

      RETURN
      END


      SUBROUTINE TSYSCO (LOCDOT, TRUPOL, STX1, STX2, STY1, STY2, TX1,
     *                   TX2, TY1, TY2, FACXX, FACYY, FACXY, FACYX,
     *                   IERR)
C-----------------------------------------------------------------------
C     Find correction factors for off-line Tsys correction with
C     averaged Tsys values.
C
C     Input:
C          LOCDOT        I     -1  => Undo on-line correction
C                               0  => Do nothing
C                               N  => Undo on-line Tsys and redo
C                                     with N averaged Tsys measurements
C          TRUPOL        I     Describes true polarizations in raw data
C                              before conversions and kludging
C                                 1: XX
C                                 2: YY
C                                 3: XX YY
C                                 4: XX YY XY YX
C                                 0: something else
C                              Validity of TRUPOL checked before coming
C                              in here.
C          STX1,STX2     R     Unedited sqrt(10*Tsys) for X on antennas
C                              1 and 2.
C          STY1,STY2     R     Unedited sqrt(10*Tsys) for Y on antennas
C                              1 and 2.
C          TX1,TX2       R     Edited sqrt(10*Tsys) for X on antennas 1
C                              and 2.
C          TY1,TY2       R     Edited sqrt(10*Tsys) for Y on antennas 1
C                              and 2.
C
C     Input/output
C          FACXX,YY,XY,YX
C                        R     Factors to apply to visiblities
C          IERR          I     0 => OK
C                              1 => Tsys was a mixture of positive and
C                              negative values.  This means something
C                              is wrong.
C
C-----------------------------------------------------------------------
      INTEGER   TRUPOL, IERR, LOCDOT
      REAL      STX1, STX2, STY1, STY2, TX1, TX2, TY1, TY2,
     *          FACXX, FACYY, FACXY, FACYX

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF ( (TRUPOL.EQ.1 .AND. STX1.GT.0.0 .AND. STX2.GT.0.0) .OR.
     *     (TRUPOL.EQ.2 .AND. STY1.GT.0.0 .AND. STY2.GT.0.0) .OR.
     *     (TRUPOL.GE.3 .AND. STX1.GT.0.0 .AND. STX2.GT.0.0 .AND.
     *                        STY1.GT.0.0 .AND. STY2.GT.0.0) ) THEN

C        On-line correction done.
         IF (LOCDOT.EQ.-1) THEN

C           Undo on-line correction and assume Tsys = 50 K
            IF (TRUPOL.NE.2) FACXX = FACXX * 500 / (STX1 * STX2)
            IF (TRUPOL.NE.1) FACYY = FACYY * 500 / (STY1 * STY2)
            IF (TRUPOL.EQ.4) THEN
               FACXY = FACXY * 500 / (STX1 * STY2)
               FACYX = FACYX * 500 / (STY1 * STX2)
            END IF
         ELSE IF (LOCDOT.GT.1) THEN

C           Redo with averaged off-line correction
            IF (TRUPOL.NE.2) FACXX = FACXX * TX1 * TX2 / (STX1 * STX2)
            IF (TRUPOL.NE.1) FACYY = FACYY * TY1 * TY2 / (STY1 * STY2)
            IF (TRUPOL.EQ.4) THEN
               FACXY = FACXY * TX1 * TY2 / (STX1 * STY2)
               FACYX = FACYX * TY1 * TX2 / (STY1 * STX2)
            END IF
         END IF

      ELSE IF ( (TRUPOL.EQ.1 .AND. STX1.LT.0.0 .AND. STX2.LT.0.0) .OR.
     *          (TRUPOL.EQ.2 .AND. STY1.LT.0.0 .AND. STY2.LT.0.0) .OR.
     *          (TRUPOL.GE.3 .AND. STX1.LT.0.0 .AND. STX2.LT.0.0 .AND.
     *                             STY1.LT.0.0 .AND. STY2.LT.0.0) ) THEN

C        On-line correction not done.

         IF (LOCDOT.EQ.-1) THEN
C           Nothing to do
            GO TO 999
         ELSE IF (LOCDOT.GT.1) THEN
            IF (TRUPOL.NE.2) FACXX = FACXX * ABS(TX1 * TX2) / 500.0
            IF (TRUPOL.NE.1) FACYY = FACYY * ABS(TY1 * TY2) / 500.0
            IF (TRUPOL.EQ.4) THEN
               FACXY = FACXY * ABS(TX1 * TY2) / 500.0
               FACYX = FACYX * ABS(TY1 * TX2) / 500.0
            END IF
         END IF
      ELSE
         MSGTXT = 'TSYSCO: INCONSISTENT Tsys VALUES, SEEK HELP!'
         CALL ATMSG (MSGTXT)
         IERR = 1
      END IF

 999  RETURN
      END



      SUBROUTINE CORAPP (TRUPOL, FACXX, FACYY, FACXY, FACYX,
     *                   NUMFRQ, VIS)
C-----------------------------------------------------------------------
C     Apply correction factors to the visibilities  that are the
C     combination of any requested sampler statitics corrections
C     and Tsys corrections.
C
C     Input:
C          TRUPOL        I     Describes true polarizations in raw data
C                              before conversions and kludging
C                                 1: XX
C                                 2: YY
C                                 3: XX YY
C                                 4: XX YY XY YX
C                                 0: something else
C                              Validity of TRUPOL checked before coming
C                              in here.
C          FACXX,YY,XY,YX
C                       R      Factors for XX,YY,XY,YX correlations
C          NUMFRQ       I      Number of channels
C     Input/output:
C          VIS()         C     Visibilities.
C
C-----------------------------------------------------------------------
      REAL FACXX, FACYY, FACXY, FACYX
      INTEGER   NUMFRQ, I, TRUPOL
      COMPLEX   VIS(*)
C-----------------------------------------------------------------------
      IF (TRUPOL.EQ.1) THEN

C        XX
         IF (FACXX.NE.1.0) THEN
            DO 10 I = 1, NUMFRQ
               VIS(I) = VIS(I) * FACXX
 10         CONTINUE
         END IF
      ELSE IF (TRUPOL.EQ.2) THEN

C        YY
         IF (FACYY.NE.1.0) THEN
            DO 20 I = 1, NUMFRQ
               VIS(I) = VIS(I) * FACYY
 20         CONTINUE
         END IF
      ELSE IF (TRUPOL.EQ.3) THEN

C        XX,YY
         IF (FACXX.NE.1.0 .OR. FACYY.NE.1.0) THEN
            DO 30 I = 1, 2*NUMFRQ, 2
               VIS(I)   = VIS(I)   * FACXX
               VIS(I+1) = VIS(I+1) * FACYY
 30         CONTINUE
         END IF
      ELSE IF (TRUPOL.EQ.4) THEN

C        XX,YY,XY,YX
         IF (FACXX.NE.1.0 .OR. FACYY.NE.1.0 .OR.
     *       FACXY.NE.1.0 .OR. FACYX.NE.1.0) THEN
            DO 40 I = 1, 4*NUMFRQ, 4
               VIS(I)   = VIS(I)   * FACXX
               VIS(I+1) = VIS(I+1) * FACYY
               VIS(I+2) = VIS(I+2) * FACXY
               VIS(I+3) = VIS(I+3) * FACYX
 40         CONTINUE
         END IF
      END IF

      RETURN
      END


      SUBROUTINE HSM (NCHAN, NPOL, VIS, VIS2)
C-----------------------------------------------------------------------
C     3-point Hanning smooth the spectrum for each polarization
C
C     Input:
C          NCHAN        I      Number of channels in VIS
C          NPOL         I      Number of polarizations in  VIS
C     Input/output:
C          VIS()        C      Visibilities.
C          VIS2()       C      Scratch buffer
C
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NPOL
      COMPLEX   VIS(NPOL*NCHAN), VIS2(NPOL*NCHAN)
CC
      INTEGER I, JP, JC, JN, L, K
C-----------------------------------------------------------------------
C     End points are not smoothed
      DO 20 I = 2, NCHAN-1
         JP = (I-2)*NPOL + 1
         JC = JP + NPOL
         JN = JC + NPOL

C        Smooth each polarization separately
         DO 10 K = 1, NPOL
            L = K - 1
            VIS2(JC+L) = 0.25*VIS(JP+L) + 0.5*VIS(JC+L) +
     *                   0.25*VIS(JN+L)
 10      CONTINUE
 20   CONTINUE

C     Copy back
      DO 30 I = NPOL+1, NPOL*(NCHAN-1)
         VIS(I) = VIS2(I)
 30   CONTINUE
C
      RETURN
      END

      SUBROUTINE LINSTK (TRUPOL, N, CHI1, CHI2, XYPHS1, XYPHS2, VIS)
C-----------------------------------------------------------------------
C     Convert linear polarizations to Stokes parameters
C
C     Input:
C       TRUPOL    I    Code for polarizations in input data
C                      3 => XX,YY
C                      4 => XX,YY,XY,YX
C                      Anything else will have already been rejected
C                      for STokes conversion
C       N         I    Number of channels
C       XYPHS1,2  R    XY phases for antennas 1 and 2 in radians
C       CHI       R    Parallactic angle in degrees.  It is assumed
C                      to be the same for all antennas, but included
C                      in the call sequence for symmetry
C
C     Input/output:
C       V         C    Visibility: on input, XX,YY,XY,YX or
C                                            XX,YY
C                                  on output  I, Q, U, V or
C                                             I
C
C-----------------------------------------------------------------------
      INTEGER N, TRUPOL
      REAL XYPHS1, XYPHS2, CHI1, CHI2
      COMPLEX VIS(*)

      COMPLEX GY1, GY2, SXX, SYY, SXY, SYX
      DOUBLE PRECISION PI
      REAL RCHI, S2CHI, C2CHI, CHI, XYPHS
      INTEGER I, J
      PARAMETER (PI = 3.141592653589793D0)
C-----------------------------------------------------------------------
C     Functions of parallactic angle
      CHI = CHI1 + 45.0
      RCHI = CHI / 57.29577951
      C2CHI = COS(2.0*RCHI)
      S2CHI = SIN(2.0*RCHI)

C     Assign XY phase difference to Y gain
      XYPHS = -(XYPHS1 + PI)
      GY1 =       CMPLX(COS(XYPHS), SIN(XYPHS))

      XYPHS = -(XYPHS2 + PI)
      GY2 = CONJG(CMPLX(COS(XYPHS), SIN(XYPHS)))

C     Convert
      IF (TRUPOL.EQ.4) THEN

C        XX,YY,XY,YX -> IQUV
         DO 10 I = 1, 4*N, 4
            SXX = VIS(I)
            SYY = VIS(I+1) / (GY1*GY2)
            SXY = VIS(I+2) /      GY2
            SYX = VIS(I+3) /  GY1
C
            VIS(I)   = (SXX + SYY) / 2.0
            VIS(I+1) = (C2CHI*(SXX-SYY) - S2CHI*(SYX+SXY)) / 2.0
            VIS(I+2) = (S2CHI*(SXX-SYY) + C2CHI*(SYX+SXY)) / 2.0
            VIS(I+3) = (SXY - SYX) / (2.0 * CMPLX(0.0,1.0))
 10      CONTINUE
      ELSE IF (TRUPOL.EQ.3) THEN

C        XX,YY -> I
         J = 1
         DO 20 I = 1, 2*N, 2
            SXX = VIS(I)
            SYY = VIS(I+1) / (GY1*GY2)
C
            VIS(J) = (SXX + SYY) / 2.0
            J = J + 1
 20      CONTINUE
      END IF

      RETURN
      END



      SUBROUTINE LINROT (TRUPOL, N, XYPHS1, XYPHS2, VIS)
C-----------------------------------------------------------------------
C     Rotate the phase of the visibilities by applying the Y XYphase
C
C     Input:
C       TRUPOL    I    COde for input polarizations
C                      3 => XX,YY   4 => XX,YY,XY,YX
C       N         I    Number of channels
C       XYPHS1,2  R    XY phases for antennas 1 and 2 in radians
C
C     Input/output:
C       V         C    Visibility: XX,YY,XY,YX
C
C-----------------------------------------------------------------------
      INTEGER N, TRUPOL
      REAL XYPHS1, XYPHS2
      COMPLEX VIS(*), GY1, GY2
CC
      REAL XYPHS
      INTEGER I
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.141592653589793D0)
C-----------------------------------------------------------------------
C     Assign XY phase difference to Y gain
      XYPHS = -(XYPHS1 + PI)
      GY1 =       CMPLX(COS(XYPHS), SIN(XYPHS))

      XYPHS = -(XYPHS2 + PI)
      GY2 = CONJG(CMPLX(COS(XYPHS), SIN(XYPHS)))

C     Apply gains
      IF (TRUPOL.EQ.4) THEN

C        XX,YY,XY,YX
         DO 10 I = 1, 4*N, 4
            VIS(I+1) = VIS(I+1) / (GY1 * GY2)
            VIS(I+2) = VIS(I+2) /        GY2
            VIS(I+3) = VIS(I+3) /  GY1
 10      CONTINUE
      ELSE IF (TRUPOL.EQ.3) THEN

C        XX,YY
         DO 20 I = 1, 2*N, 2
            VIS(I+1) = VIS(I+1) / (GY1 * GY2)
 20      CONTINUE
      END IF

      RETURN
      END


      SUBROUTINE CLPWRN (ANT_MAX, MAX_IF, IF_FREQ, DOXYFL, DOSTOK,
     *                   DOTSYS, NANT, ANTIN, MAXREJ, NREJCT, NPHCLP,
     *                   NTXCLP, NTYCLP)
C-----------------------------------------------------------------------
C     CLPWRN warns of excessive system parameter clipping.
C
C     Given:
C          ANT_MAX       I     Max no. ants allowed
C          MAX_IF        I     Max no. IFs allowed
C          IF_FREQ       R     RPFITS IF table for scan
C          DOXYFL        L     Visibilitied dropped when XY phases bad.
C          DOSTOK        L     Stokes conversion done.
C          DOTSYS        I     -1  => Undo on-line correction
C                               0  => Do nothing
C                               N  => Undo on-line Tsys and redo
C                                     with N averaged Tsys measurements
C          NANT          I     Number of antennas.
C          ANTIN(NANT)   L     True if antenna in array.
C          NREJCT(2,MAX_IF)
C                        I     Numvber of SYSCALs processed per IF
C          NPHCLP(ANT_MAX,MAX_IF)
C                        I     Number of XY phases clipped in scan per
C                              antenna per IF. Index is actual antenna
C                              number, not a pointer.
C          NTXCLP(ANT_MAX,MAX_IF)
C                        I     Number of X Tsys points clipped.
C          NTYCLP(ANT_MAX,MAX_IF)
C                        I     Number of Y Tsys points clipped.
C
C     Returned:
C          none
C
C     Called:
C          ATLOD:  {ATMSG}
C          APLSUB: {STRIM}
C
C
C-----------------------------------------------------------------------
      INTEGER   NANT, DOTSYS, MAX_IF, ANT_MAX

      LOGICAL   ANTIN(NANT), DOSTOK, DOXYFL
      INTEGER   I, J, N, NPHCLP(ANT_MAX,MAX_IF),
     *          NTXCLP(ANT_MAX,MAX_IF), NTYCLP(ANT_MAX,MAX_IF),
     *          MAXREJ, NREJCT(MAXREJ,MAX_IF)
      DOUBLE PRECISION IF_FREQ(MAX_IF)
      REAL      FRAC, FRAC2

      PARAMETER (FRAC = 0.25)

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DO 100 I = 1, MAX_IF
         IF (NREJCT(2,I).GT.0) THEN

C           XY phase differences first
            IF (DOXYFL .OR. DOSTOK) THEN
               DO 20 J = 1, NANT
                  IF (ANTIN(J)) THEN
                     FRAC2 = REAL(NPHCLP(J,I)) / REAL(NREJCT(2,I))
                     IF (FRAC2.GT.FRAC) THEN
                        WRITE (MSGTXT, 10)INT(100.0*FRAC2), 'XY phases',
     *                                     J, IF_FREQ(I)/1.0E9
 10                     FORMAT ('WARNING! (',I3,'%) ',A,
     *                          ' were bad for ant. ',I1, ' at ',
     *                          F8.5, ' GHz')
                        CALL STRIM (MSGTXT(21:), N)
                        CALL ATMSG (MSGTXT)
                     END IF
                  END IF
 20            CONTINUE
            END IF

C           Tsys
            IF (DOTSYS.GT.1) THEN
C              X Tsys
               DO 40 J = 1, NANT
                  IF (ANTIN(J)) THEN
                     FRAC2 = REAL(NTXCLP(J,I)) / REAL(NREJCT(2,I))
                     IF (FRAC2.GT.FRAC) THEN
                        WRITE (MSGTXT, 10) INT(100.0*FRAC2),
     *                     'X Tsys values', J, IF_FREQ(I)/1.0E9
                        CALL ATMSG (MSGTXT)
                     END IF
                  END IF
 40            CONTINUE

C              Y Tsys
               DO 60 J = 1, NANT
                  IF (ANTIN(J)) THEN
                     FRAC2 = REAL(NTYCLP(J,I)) / REAL(NREJCT(2,I))
                     IF (FRAC2.GT.FRAC) THEN
                        WRITE (MSGTXT, 10) INT(100.0*FRAC2),
     *                     'Y Tsys values', J, IF_FREQ(I)/1.0E9
                        CALL ATMSG (MSGTXT)
                     END IF
                  END IF
 60            CONTINUE
            END IF
         END IF
 100  CONTINUE


      RETURN
      END



      SUBROUTINE FIXSRT (IERR)
C-----------------------------------------------------------------------
C     FIXSRT recovers from data not in time sequence.
C
C     Given via common ATCTRL:
C          OUTDSK        I     Output disk number.
C
C     Given via common ATUVIO:
C          CNO           I     UV file catalogue slot number.
C
C     Given via common MAPHDR:
C          CATBLK(256)   I     Catalogue header block.
C
C     Returned via common ATUVIO:
C          TBSORT        L     True if the data is in time sequence.
C                              Reset false.
C
C     Returned:
C          IERR          I     Error status, 0 means successful.
C
C     Called:
C          ATLOD:  {ATMSG}
C          APLSUB: {CHR2H, MSGWRT, TABIO}
C          APLNOT: {RMEXT}
C
C     Algorithm:
C
C     Notes:
C       1)
C
C
C-----------------------------------------------------------------------

      INTEGER  IERR, IOBLK(256)

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C-----------------------------------------------------------------------
      TBSORT = .FALSE.

C     Fix the sort order in the image header.
      CALL CHR2H (2, '  ', 1, CATH(KITYP))

C     Delete the NX table.
      CALL RMEXT (OUTDSK, CNO, 'NX', 1, CATBLK, IOBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'FIXSRT: ERROR DELETING THE NX (SCAN INDEX) TABLE.'
         CALL ATMSG (MSGTXT)
      END IF

C     Delete the CL table.
      CALL RMEXT (OUTDSK, CNO, 'CL', 1, CATBLK, IOBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'FIXSRT: ERROR DELETING THE CL (CALIBRATION) TABLE.'
         CALL ATMSG (MSGTXT)
      END IF

C     Message to user.
      MSGTXT = 'WARNING! Data out of time sequence, NX & CL tables ' //
     *         'deleted.'
      CALL ATMSG (MSGTXT)
      MSGTXT = '         Use UVSRT and INDXR to recover.'
      CALL ATMSG (MSGTXT)


      RETURN
      END




      SUBROUTINE SYSEXT (MSC, MIF, MAN, SC_CAL, NSC, NIF, NAN,
     *                   SYSNUM, JFNUM)
C-----------------------------------------------------------------------
C     SYSEXT extracts system calibration information in a more usable
C     form.
C
C     Given:
C          MSC,MIF,MAN   I     Dimensions of SC_CAL
C          SC_CAL(,,)    R     System calibration parameters stored as
C                                 1-MSC    1-MIF     1-MAN
C                                 (par,    IF,      ant)
C                              where the NP parameters are
C                                 1    antenna number
C                                 2    IF number
C                                 3    X-Y phase difference in radians
C                                 4    SQRT(Tsys*10) for X
C                                 5    SQRT(Tsys*10) for Y
C                                 6-8  sampler statistics for X
C                                 9-11 sampler statistics for Y
C                                 12   parallactic angle in degrees
C                                 13   Flag. 0=good
C                                 14   X-Y amplitude
C          NSC,NIF,NAN   I     Dimensions of SYSNUM
C
C     Returned:
C          JFNUM()       I     This array gives the IF number for
C                              for each antenna and IF.  Thus,
C                              JFNUM(2,3) would be the IFNUM for the
C                              second antenna (whatever its actual
C                              number) and the third IF (whatever its
C                              actual value). This is used to point
C                              at the correct spot in the SYSNUM array.
C          SYSNUM(,,)    R     System calibration parameters stored as
C                                 1-NAN   1-NIF  1-NSC
C                                 (ant,     IF,   par)
C                              where the NSC parameters are
C                                 1     XY phase difference in radians
C                                 2     SQRT(10*Tsys) for X
C                                 3     SQRT(10*Tsys) for Y
C                                 4     Parallactic angle in degrees
C                                 5     SQRT(10*Tsys) for X (2nd copy)
C                                 6     SQRT(10*Tsys) for Y (2nd copy)
C                                 7     IF number
C                                 8-10  X sampler stats
C                                 11-13 Y sampler stats
C                                 14    XY amplitude
C                                 15    Flag; 0=good, 1=not good
C
C                               Values are written into locations
C                               reflecting the true antenna and IF
C                               numbers, rather than just their
C                               row numbers in the RPFITS tables.
C                               Generally, these are the same though.
C
C                               Thus the Y polarization Tsys for antenna
C                               number 4 & IFNUM=2 is in SYSNUM(4,2,3)
C
C                               The X and Y Tsys values are saved twice
C                               for possible undoing of on-line Tsys
C                               correction (the first set get edited
C                               later on).
C
C
C-----------------------------------------------------------------------

      INTEGER   IAN, IANT, MAN, NAN, NIF, JFNUM(NAN,NIF), IIF, IROUND,
     *          MSC, MIF, NSC
      REAL      SC_CAL(MSC,MIF,MAN), SYSNUM(NAN,NIF,NSC)

      INTEGER IFNUM, IFMAX
C-----------------------------------------------------------------------
C  Loop over all possible IF's (the SYSCAL groups are in fact,
C  written with only 1 IF at a time, so MIF should always be 1)
C  and antennas in SC_CAL.
      DO 20 IIF = 1, MIF
         IFMAX = 0
         DO 10 IAN = 1, MAN
C           Get antenna and IF numbers.
            IANT  = IROUND(SC_CAL(1,IIF,IAN))
            IFNUM = IROUND(SC_CAL(2,IIF,IAN))
            IF (IFNUM.LT.1 .OR. IANT.LT.1) GOTO 10

C           XY phase difference (radians).
            SYSNUM(IANT,IFNUM,1) = SC_CAL(3,IIF,IAN)

C           XY amplitude; a recent addition so check for
C           its presence
            IF (MSC.GE.14) THEN
               SYSNUM(IANT,IFNUM,14) = SC_CAL(14,IIF,IAN)
            ELSE
               SYSNUM(IANT,IFNUM,14) = -1.0
            END IF

C           SQRT(10*Tx).
            SYSNUM(IANT,IFNUM,2) = SC_CAL(4,IIF,IAN)
            SYSNUM(IANT,IFNUM,5) = SC_CAL(4,IIF,IAN)

C           SQRT(10*Ty).
            SYSNUM(IANT,IFNUM,3) = SC_CAL(5,IIF,IAN)
            SYSNUM(IANT,IFNUM,6) = SC_CAL(5,IIF,IAN)

C           Parallactic angle (degrees).
            SYSNUM(IANT,IFNUM,4) = SC_CAL(12,IIF,IAN)

C           IF number.  Storage in SYSNUM is not very useful.
            SYSNUM(IANT,IFNUM,7) = IFNUM
            JFNUM(IAN,IIF) = IFNUM
            IFMAX = MAX(IFMAX,IFNUM)

C           X Sampler statistics
            SYSNUM(IANT,IFNUM,8) = SC_CAL(6,IIF,IAN)
            SYSNUM(IANT,IFNUM,9) = SC_CAL(7,IIF,IAN)
            SYSNUM(IANT,IFNUM,10) = SC_CAL(8,IIF,IAN)

C           Y sampler statistics
            SYSNUM(IANT,IFNUM,11) = SC_CAL(9,IIF,IAN)
            SYSNUM(IANT,IFNUM,12) = SC_CAL(10,IIF,IAN)
            SYSNUM(IANT,IFNUM,13) = SC_CAL(11,IIF,IAN)

C           SYSCAL flag; a recent addition so check for
C           its presence
            IF (MSC.GE.13) THEN
               SYSNUM(IANT,IFNUM,15) = SC_CAL(13,IIF,IAN)
            ELSE
               SYSNUM(IANT,IFNUM,15) = 0.0
            END IF
 10      CONTINUE

C        We are going to assume the IF number is the same
C        regardless of antenna number for the moment.  So make
C        sure there are no zero entries which might confuse me.
         DO 15 IAN = 1, MAN
            IF (JFNUM(IAN,IIF).LE.0) JFNUM(IAN,IIF) = IFMAX
 15      CONTINUE

 20   CONTINUE

      RETURN
      END



      SUBROUTINE SYSLIS (R2D, MSC, MIF, MAN, SC_CAL, DHMS, ANTIN,
     *                   TTY, CHKPNT, OFFLIS, IERR)
C-----------------------------------------------------------------------
C     List some information from the system calibration visibilities
C     to the terminal.
C
C     Given:
C          MSC,MIF,MAN   I     Dimensions of SC_CAL.
C          SC_CAL(,,)    R     System calibration parameters, see
C                              subroutine SYSEXT for details.
C          DHMS(4)       I     UT of the scan
C          ANTIN()       L     True if antenna in array.
C          TTY(2)        I     Terminal I/O buffer
C     Given and returned:
C          CHKPNT        L     If true, ask user whether has seen enough
C                              SYSCAL info every 10 groups
C     Returned:
C          OFFLIS        L     Seen enough syscal info
C          IERR          I     0 => OK
C
C     Called:
C          ATLOD:  {ATMSG}
C
C
C-----------------------------------------------------------------------

      LOGICAL   ANTIN(*),OFFLIS, CHKPNT
      INTEGER   MSC, MIF, MAN, IROUND, I, J, IANT, IFNUM, DHMS(4)
      INTEGER   ILIST, TTY(2), IERR, FLAG
      REAL      SC_CAL(MSC,MIF,MAN)
      CHARACTER ANS*1
      DOUBLE PRECISION R2D

      INCLUDE 'INCS:DMSG.INC'
C
      SAVE ILIST
      DATA ILIST /0/
C-----------------------------------------------------------------------
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)
      MSGTXT = '    UT        IF  Ant  XYphsdiff T_sys_X  T_sys_Y' //
     *         '    Par Ang G/B'
      CALL ATMSG (MSGTXT)

C  Loop over all possible IF's and antennas
      ILIST = ILIST + 1
      DO 30 J = 1, MIF
         DO 20 I = 1, MAN
C           Get antenna and IF numbers.
            IANT  = IROUND(SC_CAL(1,J,I))
            IFNUM = IROUND(SC_CAL(2,J,I))

C           Fish out flag if available
            IF (MSC.GE.13) THEN
               FLAG = IROUND(SC_CAL(13,J,I))
            ELSE
               FLAG = 0
            END IF

C           Print system parameters.
            IF (ANTIN(IANT) .AND. IANT.GT.0 .AND. IFNUM.GT.0) THEN
               WRITE (MSGTXT, 10) DHMS, IFNUM, IANT, SC_CAL(3,J,I)*R2D,
     *            0.1*SC_CAL(4,J,I)**2, 0.1*SC_CAL(5,J,I)**2,
     *            SC_CAL(12,J,I), FLAG
 10            FORMAT (I3, I3.2, ':', I2.2, ':', I2.2, 2X, I2, 1X, I2,
     *                 4X, F7.2, 4X, F5.1, 4X, F5.1, 5X, F6.1, 2x, I1)

               CALL ATMSG (MSGTXT)
            END IF
 20      CONTINUE
 30   CONTINUE
      IF (ILIST.EQ.10 .AND. CHKPNT) THEN
         ILIST = 0
         OFFLIS = .FALSE.
C
         MSGTXT = ' '
         CALL ATMSG (MSGTXT)
         MSGTXT = 'Enough ?: <CR> (more), Q (quit listing), S '//
     *            '(stop checkpoints)'
         CALL INQSTR (TTY, MSGTXT, 1, ANS, IERR)
         IF (ANS.EQ.'Q' .OR. ANS.EQ.'q') THEN
            OFFLIS = .TRUE.
         ELSE IF (ANS.EQ.'S' .OR. ANS.EQ.'s') THEN
            CHKPNT = .FALSE.
         END IF
      END IF

      RETURN
      END



      SUBROUTINE SYSFLG (STRMIN, DOSTM, TOLSTR, TOLSTK, DATCLP, IPT,
     *                   NSTACK, NSTREAM, IST1, IST2, STACK, STREAM,
     *                   FLAG, STAKAV, BAD, CONST, NCLIP, CLIPPD)
C-----------------------------------------------------------------------
C     SYSFLG replaces discrepant points in a time series with something
C     better.  This is for a real time data stream so no information
C     up-time is available.
C
C     Given:
C       STRMIN()  L   If STRMIN(I) is true, then there is a valid
C                     datum associated with stream I
C       DOSTM     L   If true, then when filling up the first NSTACK
C                     points in the stack, put the  true stack mean into
C                     STAKAV.  If false, just put the current datum
C                     into STAKAV.  The former is probably more useful
C                     for long stacks, the latter for short stacks
C       TOLSTR    R   Clip tolerance for data stream from stack average
C       TOLSTK    R   Clip tolerance for stack from stack average
C       DATCLP    R   As well as the TOL*WIDTH algorithm, the point is
C                     finally replaced only if the point is more than
C                     ABS(DATCLP) from the stack mean.  Set to zero if
C                     you just want the TOL*WIDTH algorithm.
C       NSTACK    I   Size of stack
C       NSTREAM   I   The algorithm allows multiple and parallel data
C                     streams. This is the number of such streams.
C                     They are completely independent.  For example,
C                     5 antenna XY phase differences.  One time point
C                     for each stream is passed in each call.
C       IST1      I   Size of first dimension declared for STACK
C                     This is the maximum possible stack size
C       IST2      I   Size of second dimension declared for STACK
C                     This is the maximum number of data streams
C       FLAG      R   If 0.0, the STREAM value is good and can be
C                     used, else its quality is dubious.  This is in
C                     addition to STRMIN, which just says there
C                     is or isn't a value, but says nothing about
C                     its quality
C     Given and returned:
C       IPT()     I   Number of accumulations for each stream in time
C                     series so far.  Will be incremented if valid
C                     stream datum found
C       STACK()   R   The stack, one stack of NSTACK points for
C                     for each parallel data stream
C       STREAM()  R   The data streams.  One point (i.e. one time
C                     sample) per stream is passed in at a time.  May be
C                     modifed on output; a datum may be replaced by the
C                     mean of the current stack.
C       STAKAV()  R   Average of the current stack for each stream.
C                     Does not include current stream points unless
C                     DOSTM is true whereupon it is equal to the current
C                     datum BUT only for the first NSTACK points in
C                     the time sequence
C                     If NSTACK=1, then no accumulation is desired so
C                     put the current datum in the stack average
C                     as it is the stack average that is used for
C                     Stokes conversions and Tsys corrections.
C                     Normal accumulation NEVER puts the current datum
C                     in the average, just into the stack.
C       BAD()     L   Work array
C       NCLIP()   I   Cumulative number of clipped points from
C                     each stream
C
C     Returned:
C       CONST     L   Current stack is constant.  This usually indicates
C                     that the time sequence has jumped to a new
C                     and roughly constant value.  That value is more
C                     than the clipping tolerance so that it the
C                     mean of the old stack keeps on replacing the new
C                     values and the new value is never picked up.
C       CLIPPD()  L   True if current datum clipped, else false.
C
C     Called:
C          ATLOD:  {CLPDAT}
C
C     Algorithm:
C          For any point in the time sequence, there is a STACK of
C          NSTACK points previously accumulated.  This stack has had an
C          NSIG-width (average sum of absolute differences from mean)
C          clip applied to it, and all discrepant points in the stack
C          were replace by the clipped stack mean.  The current point
C          is then examined against the stack mean, and replaced by
C          that mean if the deviation from the mean is larger than TOL
C          stack widths. Also, the point which would be clipped in the
C          above fashion is tossed only if it is more than DATCLP
C          from the stack mean.  Finally, the stack is cycled, the first
C          point thrown away, the other NSTACK-1 points shifted down
C          one, and the last stack location filled by the curent
C          (modified perhaps) datum.
C
C          Note that the stack clip is only done early in the time
C          sequence, because later on, all discrepant points will have
C          already been removed and the new replaced values will have
C          propogated into the stack.
C
C          When no valid stream datum is present, the current
C          datum is discarded, no counters are incremented and
C          the stack and average remain unchainged.
C
C
C-----------------------------------------------------------------------
      INTEGER   IST1, IST2, NSTACK, NSTREAM

      LOGICAL   BAD(NSTACK), CLIPPD(NSTREAM), CONST(NSTREAM), DOSTM,
     *          STRMIN(NSTREAM)
      INTEGER   I, J, IERR, IPT(IST2), NCLIP(NSTREAM)
      REAL      DATCLP, MEAN, STACK(IST1,IST2), STAKAV(NSTREAM),
     *          STREAM(NSTREAM), SUM, TOLST2, TOLSTK, TOLSTR, WIDTH,
     *          FLAG(NSTREAM)
C-----------------------------------------------------------------------
C     Loop over number of streams
      DO 30 J = 1, NSTREAM
         CLIPPD(J) = .FALSE.
         CONST(J) = .FALSE.

         IF (NSTACK.EQ.1) THEN
C           Special case of NSTACK = 1.  Just put datum in average
C           and cycle stack.  Stack will never be used in this case.
            IF (STRMIN(J) .AND. FLAG(J).EQ.0.0) THEN
               STAKAV(J) = STREAM(J)
               STACK(1,J) = STREAM(J)
C
               IPT(J) = IPT(J) + 1
            ELSE
C              Leave as is if nothing good has come in.  User
C              will get last good value.
               CONTINUE
            END IF
         ELSE
C           Deal with general case of NSTACK larger than 1
            IF (IPT(J).LT.NSTACK) THEN
C              With first NSTACK points, just fill up the stack
               IF (.NOT.STRMIN(J) .OR. FLAG(J).NE.0.0) THEN
C                 No valid datum for this stream.  Stack average
C                 unchanged.
                  CONTINUE
               ELSE
                  IF (DOSTM .AND. IPT(J).GT.0) THEN
C                    Find stack average, no matter how small the stack
                     SUM = 0.0
                     DO 10 I = 1, IPT(J)
                        SUM = SUM + STACK(I,J)
 10                  CONTINUE
                     STAKAV(J) = SUM / IPT(J)
                  ELSE
C                    Put current datum in stack average
                     STAKAV(J) = STREAM(J)
                  END IF

C                 Increment counter and fill stack with new point
                  IPT(J) = IPT(J) + 1
                  STACK(IPT(J),J) = STREAM(J)
               END IF
            ELSE

C              The stack is currently full.  Work out a (possibly
C              clipped) mean and standard deviation.
               IF (IPT(J).LE.2*NSTACK) THEN
C                 Clip
                  TOLST2 = TOLSTK
               ELSE
C                 No clip
                  TOLST2 = 0.0
               END IF

               IF (.NOT.STRMIN(J) .OR. FLAG(J).NE.0.0) THEN
C                 No valid datum for this stream.  Leave all as is
                  CONTINUE
               ELSE
C                 Find (clipped) mean and width of stack
                  CALL CLPDAT (NSTACK, STACK(1,J), TOLST2, BAD, MEAN,
     *               WIDTH, IERR)
                  IF (IERR.EQ.1) CONST(J) = .TRUE.

C                 Fill stack average array
                  STAKAV(J) = MEAN

C                 Replace points discrepant from the stack mean by the
C                 mean. If couldn't get a decent width, do nothing.
C ***
C *** Need to cope with wrap of phases
C ***
                  IF (IERR.EQ.0 .AND.
     *               ABS(STREAM(J)-MEAN).GT.TOLSTR*WIDTH .AND.
     *               ABS(STREAM(J)-MEAN).GT.ABS(DATCLP)) THEN
                     STREAM(J) = MEAN
                     NCLIP(J) = NCLIP(J) + 1
                     CLIPPD(J) = .TRUE.
                  END IF

C                 Cycle stack.
                  DO 20 I = 1, NSTACK-1
                     STACK(I,J) = STACK(I+1,J)
 20               CONTINUE
                  STACK(NSTACK,J) = STREAM(J)

C                 Increment counter
                  IPT(J) = IPT(J) + 1
               END IF
            END IF
         END IF
 30   CONTINUE


      RETURN
      END


      SUBROUTINE CLPDAT (NDAT, DATA, TOL, BAD, MEAN, WIDTH, JERR)
C-----------------------------------------------------------------------
C     Perform a TOL-sigma clip on a data array, replacing the bad
C     points by the mean.  The array is unchanged if it is constant.
C
C     Given:
C         NDAT      Number of points
C         DATA      Data array
C         TOL       Tolerance level of clip.  If 0, just return
C                   mean and sigma of array with no clipping
C         BAD       If bad(i) = .true. then data(i) was replaced
C                   by the mean
C    Returned:
C         MEAN      Clipped mean.  For JERR=2 is unclipped mean.
C         WIDTH     Mean of sum of absolute differences from mean.
C                   More robust than standard deviation.
C                   For JERR=1 and 2 is 0
C         JERR      Error status
C                   0 => OK
C                   1 => Array constant
C                   2 => Too many points rejected to get clipped mean
C
C     Called:
C          ATLOD:  {STATS}
C
C
C-----------------------------------------------------------------------

      INTEGER   NDAT, JERR
      REAL      DATA(NDAT), TOL, MEAN, WIDTH
      LOGICAL   BAD(NDAT)

      REAL      CLIP, MEANI, WIDTHI
      INTEGER   I, NGOOD, IERR
C-----------------------------------------------------------------------
C     Find mean and width from all points.
      DO 10 I = 1, NDAT
         BAD(I) = .FALSE.
 10   CONTINUE
      CALL STATS (NDAT, DATA, BAD, MEAN, WIDTH, IERR)
      JERR = IERR

C     If width non-zero, tag points discrepant from mean.
      IF (IERR.EQ.0) THEN

C        Save the initial width and mean
         JERR = 0
         IF (TOL.GT.0.0) THEN
            MEANI = MEAN
            WIDTHI = WIDTH

C           Find discrepant points
            CLIP = TOL * WIDTH
            NGOOD = 0
            DO 20 I = 1, NDAT
               IF (ABS(DATA(I)-MEAN).GT.CLIP) THEN
                  BAD(I) = .TRUE.
               ELSE
                  NGOOD = NGOOD + 1
               END IF
 20         CONTINUE

C           Get new mean and width if enough points
            IF (NGOOD.LE.1) THEN

C              Too many points rejected to get clipped mean, just return
C              initial values
               JERR = 2
               MEAN = MEANI
               WIDTH = WIDTHI
            ELSE
               CALL STATS (NDAT, DATA, BAD, MEAN, WIDTH, IERR)

C              Replace discrepant points by mean if possible,
C              else leave array unchanged.
               IF (IERR.LE.1) THEN
                  DO 30 I = 1, NDAT
                     IF (BAD(I)) DATA(I) = MEAN
 30               CONTINUE
               END IF
            END IF
         END IF
      END IF

      RETURN
      END


      SUBROUTINE DRIFT (NANT, NSTAK, MAXST, NCA, NSYS, WORK, MINSTK,
     *   TOL, STAK, STREAM, ANTIN, SYSFLG, ITOT, SUMDIF, SUMWID)
C-----------------------------------------------------------------------
C     As part of trying to catch drifting phase or Tsys values and
C     accumulate sums (over all antennas) of (datum - stack_mean)
C     and sums of stack widths.  If the datum is too discrepant from
C     stack mean, then it is not included in the sums.
C
C     Given:
C          NANT          I     Number of antennas.
C          NSTAK         I     Size of stack
C          MAXST         I     Size of first dimension declared for
C                              STAK.  This is the maximum possible stack
C                              size.
C          NCA           I     Size of second dimension declared for
C                              STAK.  This is the maximum number of
C                              antennas.
C          NSYS()        I     Number of accumulations for each antenna
C                              in time series so far.
C          WORK()        L     Work array.
C          MINSTK        I     Minimum size of stack allowed to perform
C                              function of this subroutine.
C          TOL           R     Points more than TOL*WIDTH from the stack
C                              average are not included in the sums.
C          STAK()        R     The stacks, one stack of NSTAK points for
C                              for each antenna.
C          STREAM()      R     The data streams, one point per antenna.
C          ANTIN()       L     If ANTIN(I) is true, then there is a
C                              valid datum associated with antenna I.
C          SYSFLG()      R     If 0.0, SYSCAL value good, else bad.
C
C     Given and returned:
C          ITOT          I     Number of points summed.
C          SUMDIF        R     Sum of (dataum - stack_mean) over all
C                              antennas.
C          SUMWID        R     Sum of stack widths overall antennas.
C
C-----------------------------------------------------------------------
      INTEGER   NANT

      LOGICAL   ANTIN(NANT), WORK(NANT)
      INTEGER   IERR, ITOT, J, MAXST, MINSTK, MSTAK, NCA, NSTAK, NSYS(*)
      REAL      DIFF, MEAN, STAK(MAXST,NCA), STREAM(NANT), SUMDIF,
     *          SUMWID, TOL, WIDTH, SYSFLG(NANT)
C-----------------------------------------------------------------------
C     Loop over number of antennas
      DO 10 J = 1, NANT
         IF (ANTIN(J) .AND. SYSFLG(J).EQ.0.0) THEN
C           Compare datum with stack average and width if sufficient
C           points accumulated so far.
            IF (NSYS(J).GE.MINSTK) THEN
               MSTAK = MIN(NSTAK, NSYS(J))
               CALL STATS (MSTAK, STAK(1,J), WORK, MEAN, WIDTH, IERR)
               IF (IERR.EQ.0 .AND. WIDTH.GT.0.0) THEN
C                 Don't include severely discrepant points
                  DIFF = ABS(STREAM(J) - MEAN)
                  IF (DIFF.LT.ABS(TOL*WIDTH)) THEN
                     SUMDIF = SUMDIF + DIFF
                     SUMWID = SUMWID + WIDTH
                     ITOT = ITOT + 1
                  END IF
               END IF
            END IF
         END IF
 10   CONTINUE

      RETURN
      END



      SUBROUTINE STATS (NDAT, DATA, BAD, MEAN, WIDTH, IERR)
C-----------------------------------------------------------------------
C     Work out a mean and standard deviation of an array.
C
C     Given:
C         NDAT      Number of points.
C         DATA      Data array.
C         BAD       If BAD(I) is true then the Ith location of DATA
C                   should not be included in the calculations.
C
C     Returned:
C         MEAN      Mean of array
C         WIDTH     The mean of the absolute differences of the array
C                   from the mean.  More robust than standard deviation
C                   MEAN and WIDTH valid only for IERR = 0 or 1
C         IERR      Error status
C                   0 => OK
C                   1 => Array is constant
C                   2 => All input points flagged bad
C
C     Called:
C          none
C
C
C-----------------------------------------------------------------------
      INTEGER   NDAT

      LOGICAL   BAD(NDAT), CONST
      INTEGER   I, J, IERR
      REAL      DATA(NDAT), FIRST, MEAN, SUM, SUMDIF, WIDTH
C-----------------------------------------------------------------------
      SUM = 0.0
      SUMDIF = 0.0
      J = 0
      IERR = 0
      CONST = .TRUE.
      FIRST = DATA(1)

C     Find sum
      DO 10 I = 1, NDAT
         IF (.NOT.BAD(I)) THEN
            J = J + 1
            SUM = SUM + DATA(I)
            IF (FIRST.NE.0.0) THEN
               IF (ABS((DATA(I)-FIRST)/FIRST).GT.1.0E-6) CONST = .FALSE.
            ELSE

C              Unlikely to be here, but if so, arbitrarily claim
C              array is not constant (always true at some bit level !!)
               CONST = .FALSE.
            END IF
         END IF
 10   CONTINUE

C     Work out mean and width
      IF (J.EQ.0) THEN
C        No valid points
         IERR = 2
      ELSE
         IF (CONST) THEN
C           Array constant
            IERR = 1
            MEAN = DATA(1)
            WIDTH = 0.0
         ELSE
C           Compute mean and width
            MEAN = SUM / J
            SUMDIF = 0.0
            DO 20 I = 1, NDAT
               IF (.NOT.BAD(I)) SUMDIF = SUMDIF + ABS(DATA(I) - MEAN)
 20         CONTINUE
            WIDTH = SUMDIF / J
         END IF
      END IF

      RETURN
      END



      SUBROUTINE TXTOPN (TLUN, TFIND, IERR)
C-----------------------------------------------------------------------
C     Open three text files (append mode) in the FITS area for the
C     SYSCAL numbers (XY phase, TsysX and TsysY)
C
C   Output:
C     TLUN     I      Luns for files
C     TFIND    I      Pointers to FTAB
C     IERR     I      0 => OK
C
C-----------------------------------------------------------------------
      INTEGER TLUN(*), TFIND(*), IERR
C
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER ICODE, IROUND, ITRIM, IL
      CHARACTER SYSFNAM*20
CC
      INTEGER IPL
      CHARACTER*4 IFORM(4)
      CHARACTER TRAIL*4
      DATA IFORM /'(I1)', '(I2)', '(I3)', '(I4)'/
C-----------------------------------------------------------------------
      ICODE = IROUND(XCPARM(9))
      IPL = INT(LOG10(REAL(NLUSER))) + 1
      WRITE (TRAIL, IFORM(IPL)) NLUSER
C
      IF (ICODE.EQ.4) THEN
         SYSFNAM = 'FITS:XYAMP_'//TRAIL
      ELSE IF (ICODE.EQ.3) THEN
         SYSFNAM = 'FITS:YSSNEG_'//TRAIL
      ELSE IF (ICODE.EQ.2) THEN
         SYSFNAM = 'FITS:XSSNEG_'//TRAIL
      ELSE
         SYSFNAM = 'FITS:XYPHS_'//TRAIL
      END IF
      IL = ITRIM(SYSFNAM)
C
      CALL ZTXOPN ('WRIT', TLUN(1), TFIND(1), SYSFNAM,
     *             .TRUE., IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TXTOPN: ERROR OPENING TEXT FILE SYSXY.TXT '
     *            //'IN FITS AREA'
         CALL ATMSG (MSGTXT)
         GOTO 999
      ELSE
         MSGTXT = 'Opened text file '//SYSFNAM(1:IL)//
     *            ' in FITS area'
         CALL ATMSG (MSGTXT)
      END IF

C
      IF (ICODE.EQ.4) THEN
         SYSFNAM = 'FITS:XYPHS_'//TRAIL
      ELSE IF (ICODE.EQ.3) THEN
         SYSFNAM = 'FITS:YSSZER_'//TRAIL
      ELSE IF (ICODE.EQ.2) THEN
         SYSFNAM = 'FITS:XSSZER_'//TRAIL
      ELSE
         SYSFNAM = 'FITS:TSYSX_'//TRAIL
      END IF
      IL = ITRIM(SYSFNAM)
C
      CALL ZTXOPN ('WRIT', TLUN(2), TFIND(2), SYSFNAM,
     *             .TRUE., IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR OPENING TEXT FILE '//SYSFNAM(1:IL)//
     *            ' IN FITS AREA'
         CALL ATMSG (MSGTXT)
         GOTO 999
      ELSE
         MSGTXT = 'Opened text file '//SYSFNAM(1:IL)//
     *            ' in FITS area'
         CALL ATMSG (MSGTXT)
      END IF
C
      IF (ICODE.EQ.4) THEN
         SYSFNAM = 'FITS:CHI_'//TRAIL
      ELSE IF (ICODE.EQ.3) THEN
         SYSFNAM = 'FITS:YSSPOS_'//TRAIL
      ELSE IF (ICODE.EQ.2) THEN
         SYSFNAM = 'FITS:XSSPOS_'//TRAIL
      ELSE
         SYSFNAM = 'FITS:TSYSY_'//TRAIL
      END IF
      IL = ITRIM(SYSFNAM)
C
      CALL ZTXOPN ('WRIT', TLUN(3), TFIND(3), SYSFNAM,
     *             .TRUE., IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR OPENING TEXT FILE '//SYSFNAM(1:IL)//
     *            ' IN FITS AREA'
         CALL ATMSG (MSGTXT)
         GOTO 999
      ELSE
         MSGTXT = 'Opened text file '//SYSFNAM(1:IL)//
     *            ' in FITS area'
         CALL ATMSG (MSGTXT)
      END IF
C
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)
C
 999  RETURN
      END



      SUBROUTINE TRMOPN  (TTY, IERR)
C-----------------------------------------------------------------------
C     Open the terminal for interaction with the user
C
C   Input:
C     TTY        I      LUn for terminal
C   Output:
C     IERR       I      0 => OK
C
C-----------------------------------------------------------------------
      INTEGER TTY(2), IERR
C
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER CNAME*48
C-----------------------------------------------------------------------
      CALL ZOPEN (TTY(1), TTY(2), 1, CNAME, .FALSE., .TRUE.,
     *            .TRUE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 100)  IERR
  100    FORMAT ('Unable to open terminal, IERR = ', I3)
         CALL ATMSG (MSGTXT)
      END IF
C
      RETURN
      END


      SUBROUTINE TXTWRT (FIRST, TLUN, TFIND, IFILE, ISCAN, EOF,
     *                   NTXTR, NTXTW, IERR)
C-----------------------------------------------------------------------
C     Process the SYSCAL groups only, writing some useful information
C     into three text files
C
C     Input:
C          FIRST         L     True for first time in here
C          TLUN          I     Luns for text files
C          TFIND         I     Pointers for FTAB
C          IFILE         I     File number
C          ISCAN         I     Scan number
C     Input/output
C          NTXTR,W       I     Number of SYSCAL records read and written
C     Output:
C          EOF           L     True if EOF reached
C          IERR          I     0 => OK
C
C-----------------------------------------------------------------------
      INTEGER IFILE, ISCAN, TLUN(3), TFIND(3), IERR, NTXTR, NTXTW
      LOGICAL EOF, FIRST
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
C
      DOUBLE PRECISION  JDAY, JDAY0, OFFTIM
      REAL TOL, SYSNUM(ANT_MAX,MAX_IF,NSYSCP), SYSFLG(ANT_MAX)
      INTEGER   JSTAT, IIF, I, J, K, IROUND, ILEN, DHMS(4), ICODE, IFRQ,
     *  JFNUM(ANT_MAX,MAX_IF)

      CHARACTER ALINE*132
      LOGICAL STARTS
      INTEGER ISRC

C
      PARAMETER (TOL = 1.0E3)
      SAVE JDAY0
C-----------------------------------------------------------------------
      WRITE (MSGTXT, 10) IFILE, ISCAN
 10   FORMAT ('File:',I3,',  Scan:',I4)
      CALL ATMSG (MSGTXT)

C     Julian date at the start of the observation or for current scan.
      IF (FIRST) THEN
         CALL JULDAY (DATOB8, JDAY0)
         FIRST = .FALSE.
      END IF
      CALL JULDAY (DATOB8, JDAY)
      STARTS = .TRUE.
      IERR = 0

      IF (IF_FOUND) THEN
         CALL IFMACH (IERR)
         IF (IERR.NE.0) GOTO 999
      ELSE
         MSGTXT = 'TXTWRT: NO FREQUENCY TABLE FOUND FOR THIS SCAN, ' //
     *            'ALL FREQS. ACCEPTED'
         CALL ATMSG (MSGTXT)
         GO TO 999
      END IF

C     Proceed if there are some selected frequencies
      IF (NIFSEL.EQ.0) THEN
         MSGTXT = 'TXTWRT: no frequencies selected for this scan'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF

C     Match user specified sources
      IF (SU_FOUND) THEN
         CALL SUMACH (IERR)
         IF (IERR.NE.0) GOTO 999
      ELSE
         MSGTXT = 'TXTWRT: NO SOURCE TABLE FOUND FOR THIS SCAN, '//
     *            'SKIPPING IT'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF


C     Proceed if there are some selected sources.
      IF (NSUSEL.EQ.0) THEN
         MSGTXT = 'TXTWRT: no sources selected for this scan'
         CALL ATMSG (MSGTXT)
         GOTO 999
      END IF


C     Deselect all sources and frequencies
      DO 30 IFRQ = 1, MAX_IF
         FQTAGS(IFRQ) = -1
         DO 20 ISRC = 1, MAX_SU
            SUTAGS(ISRC,IFRQ) = -1
 20      CONTINUE
 30   CONTINUE

C     Set FQTAGS entries for wanted frequencies; Loop over number
C     of selected simultaneous frequency groups
      DO 34 I = 1, NIFSEL

C        Loop over simultaneous frequencies in this group
         DO 32 J = 1, SELNIF(I)

C           Find RPFITS IF table entry number
            IFRQ = SELIF(I,J)

C           Set translation from IFNUM to FREQID as being wanted
            FQTAGS(IFRQ) = 1
 32      CONTINUE
 34   CONTINUE

C     Set SUTAGS entries for wanted sources and frequencies.
C     Loop over selected sources
      DO 40 I = 1, NSUSEL

C        Find RPFITS SU table entry
         ISRC = SELSRC(I)

C        Loop over number of frequency groups
         DO 38 J = 1, NIFSEL

C           Loop over number of frequencies in this group
            DO 36 K = 1, SELNIF(J)

C              Find RPFITS IF table entry
               IFRQ = SELIF(J,K)

C              Mark this source at this frequency as wanted.
               IF (FQTAGS(IFRQ).EQ.1) SUTAGS(ISRC,IFRQ) = 1
 36         CONTINUE
 38      CONTINUE
 40   CONTINUE


C     Sets what we write into files
      ICODE = IROUND(XCPARM(9))

C     Loop through the scan fishing out the SYSCAL info
 50   CONTINUE
C        Get the next visibility
         JSTAT = 0
         CALL RPFITSIN (JSTAT, VIS, WEIGHT, BASELN, UT, U, V, W, FLAG,
     *      BIN, IFNUM, SRCNUM)
         IERR = 0
         IF (JSTAT.NE.0) THEN
            IF (JSTAT.EQ.-1) THEN
               IERR = 1
               MSGTXT = 'TXTWRT: SEVERE ERROR WHILE READING DATA'
               CALL ATMSG (MSGTXT)
            ELSE IF (JSTAT.EQ.1) THEN
C              Note that the next attempt to read a header will read the
C              one just encountered here, not the one beyond.
            ELSE IF (JSTAT.EQ.2) THEN
               MSGTXT = 'Normal end of scan encountered'
               CALL ATMSG (MSGTXT)
            ELSE IF (JSTAT.EQ.3) THEN
C              End-of-file encountered.
               EOF = .TRUE.
C
               WRITE (MSGTXT, 100) IFILE
 100           FORMAT ('Finished reading file number', I3,'.')
               CALL ATMSG (MSGTXT)
            ELSE IF (JSTAT.EQ.4) THEN
               MSGTXT = 'Flagging table encountered and ignored'
               CALL ATMSG (MSGTXT)
            ELSE IF (JSTAT.EQ.5) THEN
               MSGTXT = 'Invalid data input, skipping to next scan'
               CALL ATMSG (MSGTXT)
            END IF
            GO TO 999
         END IF

C        Process system calibration records.
         IF (BASELN.EQ.-1) THEN
            NTXTR = NTXTR + 1
            OFFTIM = (JDAY - JDAY0) + SC_UT/86400D0

C           Tell user time of start of scan
            IF (STARTS) THEN
               CALL TIMCON (OFFTIM, DHMS)
               WRITE (MSGTXT, 160) DATOBS, DHMS
 160           FORMAT ('Date: ',A,'  UT start:',I3,I3.2,':',
     *                  I2.2,':',I2.2)
               STARTS = .FALSE.
               CALL ATMSG (MSGTXT)
            END IF

C           Reject for time selection
            IF (OFFTIM.LT.TIMRNG(1) .OR. OFFTIM.GT.TIMRNG(2)) GOTO 50

C           Extract the truth
            CALL SYSEXT (SC_Q, SC_IF, SC_ANT, SC_CAL, NSYSCP, MAX_IF,
     *                   ANT_MAX, SYSNUM, JFNUM)

C           Fish out source ID for this SYSCAL group.  Old
C           data has no selection criterion.
            SRCNUM = 1
            IF (JDAY0.GE.JULSRC) SRCNUM = SC_SRCNO
            IF (SRCNUM.LT.1 .OR. SRCNUM.GT.MAX_SU) GOTO 50

C           Loop for each IF number in the syscal array.
            DO 400 IIF = 1, SC_IF
C              Assume IFNUM same for all antennas; zeroes checked
C              in SYSEXT
               IFNUM = JFNUM(1,IIF)

C              See if we want this one.  If new data, can make source
C              and frequency selection, for old data, can make only
C              frequency selection
               IF (IFNUM.GE.1 .AND. IFNUM.LE.MAX_IF .AND.
     *            ((JDAY0.GE.JULSRC .AND. SUTAGS(SRCNUM,IFNUM).EQ.1)
     *                              .OR.
     *             (JDAY0.LT.JULSRC .AND. FQTAGS(IFNUM).EQ.1))) THEN
C
                  IF (SC_Q.GE.13) THEN
                     DO 165 I = 1, ANT_MAX
                        SYSFLG(I) = SYSNUM(I,IFNUM,15)
 165                 CONTINUE
                  ELSE
                     DO 166 I = 1, ANT_MAX
                        SYSFLG(I) = 0.0
 166                 CONTINUE
                  END IF
C
                  IF (ICODE.EQ.4) THEN
C                    XY amplitude
                     CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,14),
     *                 SYSFLG, 4, IF_FREQ(IFNUM), ALINE, ILEN)
                  ELSE IF (ICODE.EQ.3) THEN
C                    Y samplers neg
                     CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,11),
     *                 SYSFLG, 3, IF_FREQ(IFNUM), ALINE, ILEN)
                  ELSE IF (ICODE.EQ.2) THEN
C                    X samplers neg
                     CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,8),
     *                 SYSFLG, 3, IF_FREQ(IFNUM), ALINE, ILEN)
                  ELSE
C                    XY phase
                     CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,1),
     *                 SYSFLG, 1, IF_FREQ(IFNUM), ALINE, ILEN)
                  END IF
                  CALL ZTXIO ('WRIT', TLUN(1), TFIND(1), ALINE(1:ILEN),
     *                         IERR)
C
                  IF (IERR.EQ.0) THEN
                     IF (ICODE.EQ.4) THEN
C                       XY phase
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,1),
     *                    SYSFLG, 1, IF_FREQ(IFNUM), ALINE, ILEN)
                     ELSE IF (ICODE.EQ.3) THEN
C                       Y samplers zero
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,12),
     *                    SYSFLG, 3, IF_FREQ(IFNUM), ALINE, ILEN)
                     ELSE IF (ICODE.EQ.2) THEN
C                       X samplers zero
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,9),
     *                    SYSFLG, 3, IF_FREQ(IFNUM), ALINE, ILEN)
                     ELSE
C                       X Tsys
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,2),
     *                    SYSFLG, 2, IF_FREQ(IFNUM), ALINE, ILEN)
                     END IF
                     CALL ZTXIO ('WRIT', TLUN(2), TFIND(2),
     *                            ALINE(1:ILEN), IERR)
                  END IF
C
                  IF (IERR.EQ.0) THEN
                     IF (ICODE.EQ.4) THEN
C                       Parallactic angle
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,4),
     *                    SYSFLG, 4, IF_FREQ(IFNUM), ALINE, ILEN)
                     ELSE IF (ICODE.EQ.3) THEN
C                       Y samplers pos
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,13),
     *                    SYSFLG, 3, IF_FREQ(IFNUM), ALINE, ILEN)
                     ELSE IF (ICODE.EQ.2) THEN
C                       X samplers pos
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,10),
     *                    SYSFLG, 3, IF_FREQ(IFNUM), ALINE, ILEN)
                     ELSE
C                       Y Tsys
                        CALL TXTPUT (OFFTIM, SC_ANT, SYSNUM(1,IFNUM,3),
     *                    SYSFLG, 2, IF_FREQ(IFNUM), ALINE, ILEN)
                     END IF
                     CALL ZTXIO ('WRIT', TLUN(3), TFIND(3),
     *                            ALINE(1:ILEN), IERR)
                  END IF
C
                  IF (IERR.NE.0) THEN
                     MSGTXT = 'ERROR WRITING TEXT FILES'
                     CALL ATMSG (MSGTXT)
                     GOTO 999
                  END IF
C
                  NTXTW = NTXTW + 1
               END IF
 400        CONTINUE
         END IF
      GO TO 50
C

 999  RETURN
      END

      SUBROUTINE TXTPUT (TIME, N, RNUM, SYSFLG, ITYPE, FREQ,
     *                   ALINE, ILEN)
C-----------------------------------------------------------------------
C     Write some SYSCAL numbers into a text string
C
C   Input:
C      TIME    D      Time of SYSCAL group (days)
C      N       I      Number of numbers
C      RNUM    R      Numbers
C      SYSFLG  R      Flags, 0 => good
C      ITYPE   I      1 for XY phase, 2 for sqrt(10*Tsys),
C                     3 for sampler statistics
C                     4 for XY amplitude and parallactic angle
C      FREQ    D      Frequency (Hz)
C   Output:
C      ALINE   C      Text string
C      ILEN    I      Length of string
C
C-----------------------------------------------------------------------
      INTEGER N, ITYPE, ILEN
      REAL RNUM(N), SYSFLG(N)
      DOUBLE PRECISION FREQ, TIME
      CHARACTER ALINE*(*)
CC
      INTEGER I, IPT, ITRIM
      REAL TMP
C-----------------------------------------------------------------------
      ALINE = ' '
      WRITE (ALINE, '(F11.6,1X,F7.2)') TIME, FREQ/1.0E6
      IPT = 21
C
      DO 100 I = 1, N
         IF (ITYPE.EQ.1) THEN

C           Write XY phase difference
            WRITE(ALINE(IPT:), '(F7.2,1x,I1)') 57.29577951*RNUM(I),
     *                                         NINT(SYSFLG(I))
            IPT = IPT + 10
         ELSE IF (ITYPE.EQ.2) THEN

C           Write Tsys
            TMP = 0.1*RNUM(I)**2
            IF (RNUM(I).LT.0.0) TMP = -TMP
            WRITE(ALINE(IPT:), '(F7.2,1x,I1)') TMP, NINT(SYSFLG(I))
            IPT = IPT + 11
         ELSE IF (ITYPE.EQ.3) THEN

C           Write sampler stats
            WRITE(ALINE(IPT:), '(F6.2,1x,I1)') RNUM(I),
     *                                         NINT(SYSFLG(I))
            IPT = IPT + 9
         ELSE IF (ITYPE.EQ.4) THEN

C           Write XY amplitude, parallactic angle
            WRITE (ALINE(IPT:), '(F7.2,1X,I1)') RNUM(I),
     *                                         NINT(SYSFLG(I))
            IPT = IPT + 10
         END IF
100   CONTINUE
      ILEN = ITRIM(ALINE)
C
      RETURN
      END


      SUBROUTINE TXTCLS (NTXTR, NTXTW, TLUN, TFIND)
C-----------------------------------------------------------------------
C     Close text files
C
C  Input:
C    NTXTR,W  I   Number of SYSCAL records read and written
C    TLUN     I   LUNs
C    TFIND    I   FTAB pointers
C
C-----------------------------------------------------------------------
      INTEGER TLUN(3), TFIND(3), NTXTR, NTXTW
C
      INCLUDE 'INCS:DMSG.INC'
      INTEGER IERR, I
C-----------------------------------------------------------------------
      WRITE (MSGTXT,50) NTXTR
 50   FORMAT ('Read ', I6, ' SYSCAL records')
      CALL ATMSG (MSGTXT)
      WRITE (MSGTXT,60) NTXTW
 60   FORMAT ('Wrote', I6, ' SYSCAL records')
      CALL ATMSG (MSGTXT)
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)
C
      DO 100 I = 1, 3
         CALL ZTXCLS (TLUN(I), TFIND(I), IERR)
 100  CONTINUE
C
      RETURN
      END

      SUBROUTINE SEVREP (NSEVER, SEVERE)
C-----------------------------------------------------------------------
C     SEVREP tells user about nasty errors that were encountered
C     in this application of ATLOD.
C
C     Given:
C          NSEVER        I     NUmber of severe errors in file
C          SEVERE        C*80  Errors to reprort.
C
C
C-----------------------------------------------------------------------
      INTEGER NSEVER, I
      CHARACTER*80 SEVERE(*)

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)
      MSGTXT = 'THE FOLLOWING SEVERE ERRORS OCCURRED DURING '//
     *         'THIS RUN OF ATLOD'
      CALL ATMSG (MSGTXT)
      MSGTXT = ' '
      CALL ATMSG (MSGTXT)
C
      DO 30 I = 1, NSEVER
         MSGTXT = SEVERE(I)
         CALL ATMSG (MSGTXT)
 30   CONTINUE

      RETURN
      END


      SUBROUTINE ATTELL (IERR)
C-----------------------------------------------------------------------
C   ATTELL checks the TELL file to see if anything is waiting for ATLOD
C   If so, it picks up the parms (via GTTELL), interprets them, and
C   sends them back to the rest of the task to handle.
C
C   In/out:
C
C   Output:   IRET    I     0 => okay
C                           2 => TELL ordered QUIT gracefully
C                           3 => TELL ordered ABORT
C-----------------------------------------------------------------------
      CHARACTER OPTELL*4
      INTEGER   NPARMS, SCRTCH(256), IERR, IROUND, I, J, K, N
      REAL XNSK, XNF, XBC, XNC, XTIM(8), TIMRNG2(2), XSF(30),
     *  XSIFN(5)
      HOLLERITH XSRC(4,30)

      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /ATTLL/ XNSK, XNF, XBC, XNC, XSRC, XTIM, XSF, XSIFN
C-----------------------------------------------------------------------
C     Check TELL file
      NPARMS = 167
      CALL GTTELL (NPARMS, OPTELL, XNSK, SCRTCH, IERR)
      WRITE (MSGTXT,1000) OPTELL
      IF (IERR.NE.0) CALL ATMSG (MSGTXT)

      IF (IERR.EQ.0) THEN

C        Either error, or OK but no parameters
         GOTO 999
      ELSE IF (IERR.EQ.1) THEN

C        Got parameters successfully
         CONTINUE
      ELSE IF (IERR.EQ.2) THEN

C        TELL orders QUIT
         GOTO 999
      ELSE IF (IERR.EQ.3) THEN

C        TELL orders ABORT
         GOTO 999
      END IF
      IERR = 0

C     Counters
      IF (XNSK.GT.0.0) NSKIP = IROUND(XNSK)
      IF (XNF.GT.0.0) NFILES = IROUND(XNF)
      IF (XBC.GT.0.0) BSCAN = IROUND(XBC)
      IF (XNC.GT.0.0) NSCANS = IROUND(XNC)
      ESCAN = BSCAN + NSCANS - 1

C     Sources
      NSOURC = 0
      DOSWNT = .TRUE.
      DO 30 J = 1, 30
         SOURCS(J) = ' '
         CALL H2CHR (16, 1, XSRC(1,J), SOURCS(J))
         IF (SOURCS(J).NE.' ') THEN
            CALL STRIM (SOURCS(J), N)
            IF (SOURCS(J)(1:1).EQ.'-') THEN
               DOSWNT = .FALSE.
               SOURCS(J) = SOURCS(J)(2:)
            END IF

C           Make sure its not already in the list.
            DO 20 K = 1, NSOURC
               IF (SOURCS(J).EQ.SOURCS(K)) GO TO 30
 20         CONTINUE
            NSOURC = NSOURC + 1
            IF (J.GT.NSOURC) SOURCS(NSOURC) = SOURCS(J)
         END IF

C        Just for safety's sake.
         IF (NSOURC.EQ.0) DOSWNT = .FALSE.
 30   CONTINUE

C     Time range
      TIMRNG2(1) = ((XTIM(4)/60.0 + XTIM(3))/60.0 + XTIM(2))/24.0
      TIMRNG2(1) = TIMRNG2(1) + XTIM(1)
      TIMRNG2(2) = ((XTIM(8)/60.0 + XTIM(7))/60.0 + XTIM(6))/24.0
      TIMRNG2(2) = TIMRNG2(2) + XTIM(5)
      IF (TIMRNG2(1).EQ.0.0) TIMRNG2(1) = -9999.0
      IF (TIMRNG2(2).EQ.0.0) TIMRNG2(2) = +9999.0
      IF (TIMRNG2(2).LE.TIMRNG2(1)) THEN
         MSGTXT = 'ATTELL: INVALID TIME RANGE; NO CHANGE'
         CALL ATMSG (MSGTXT)
         IERR = 0
      ELSE
         TIMRNG(1) = TIMRNG2(1)
         TIMRNG(2) = TIMRNG2(2)
      END IF

C     Frequency selection
      NFREQS = 0
      DO 40 I = 1, 30
         FREQS(I) = 0.0
         IF (XSF(I).GT.0.0) THEN
            NFREQS = NFREQS + 1
            FREQS(NFREQS) = XSF(I) * 1.0D6
         END IF
 40   CONTINUE


C     IF selection
      NIFS = 0
      DO 42 I = 1, 5
         IFS(I) = 0
         IF (XSIFN(I).GT.0.0) THEN
            NIFS = NIFS + 1
            IFS(NIFS) = IROUND(XSIFN(I))
         END IF
 42   CONTINUE

C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Received ',A4,' operation from TELL')
      END



C***********************************************************************
C     The following code is from Warwick Wilson.  It is the
C     same as the on-line code, hence the non-standard style
C

      REAL FUNCTION TWOBIT_GAIN_ADJUST(SSEXP, N1, Z1, P1, N2, Z2, P2)
C----------------------------------------------------------------------
C
C      Finds gain correction factor to be applied to data whose
C      gain has been calculated on the assumption that the
C      sampler statistics were either set on :
C
C      17.1, 50.0, 17.1 percent, for SSEXP = 17.1
C                  --OR--
C      17.3, 50.0, 17.3 percent, for SSEXP = 17.3
C
C      whereas they were actually n1, z1, p1 and n2, z2, p2 percent
C      on inputs 1,2.  ( n=neg, z=zero, p=pos )
C
C      The data should be multiplied by twobit_gain_adjust
C      to obtain the corrected data.
C
C----------------------------------------------------------------------

C PARAMETERS
      REAL      N1, Z1, P1, N2, Z2, P2, SSEXP

C EXTERNAL FUNCTIONS
      REAL      GAIN_PARAM, TWOBIT_GAIN_R0

C LOCAL VARIABLES
      REAL      QN1, QZ1, QP1, QN2, QZ2, QP2
      REAL      A

C BEGIN

      QN1 = GAIN_PARAM( N1 )
      QZ1 = GAIN_PARAM( Z1 )
      QP1 = GAIN_PARAM( P1 )

      QN2 = GAIN_PARAM( N2 )
      QZ2 = GAIN_PARAM( Z2 )
      QP2 = GAIN_PARAM( P2 )

C In the following, a is the gain that was applied on line.
C It is calculated from
C  a = average correlator count / digital correlator gain at
C      zero correlatoion

      IF (SSEXP.GT.17.2) THEN
C          5.444705 = 6.19 / 1.1368844
         A = 5.444705
      ELSE IF (SSEXP.GT.17.0) THEN
C          5.392175663 = 6.13 / 1.1368324
         A = 5.392175663
      END IF

C
      TWOBIT_GAIN_ADJUST = A /
     *     TWOBIT_GAIN_R0( QN1, QZ1, QP1, QN2, QZ2, QP2 )

      RETURN

      END


      REAL FUNCTION GAIN_PARAM( LEVEL_PERCENT )
C----------------------------------------------------------------------
C
C      Gets "gain parameter" - i.e. parameter useful for calculating
C      gain of 2-bit digital correlator for uncorrelated inputs
C      where one of the sampler statistics is level_percent.
C
C----------------------------------------------------------------------

C PARAMETERS
      REAL      LEVEL_PERCENT

C EXTERNAL FUNCTIONS
      REAL      GAUSS_LEVEL

C LOCAL VARIABLES
      REAL      X

C BEGIN

C Find level ( RMS = 1.0 ) appropriate to this statistic
      X = GAUSS_LEVEL( LEVEL_PERCENT / 100.0 )

      GAIN_PARAM = EXP( -X * X / 2.0 )

      END




      REAL FUNCTION GAUSS_LEVEL( FRACTION_ABOVE_LEVEL )
C----------------------------------------------------------------------
C
C      Assuming Gaussian statistics, estimates the level given
C       the probability of being above this level, i.e.
C       the fraction of samples above this level.
C
C      Ref.      Approximation formulae - max. error 4.5E-04
C
C----------------------------------------------------------------------

C PARAMETERS
      REAL            FRACTION_ABOVE_LEVEL

C CONSTANTS
      REAL            C0, C1, C2, D1, D2, D3

      PARAMETER      ( C0 = 2.515517 )
      PARAMETER      ( C1 = 0.802853 )
      PARAMETER      ( C2 = 0.010328 )
      PARAMETER      ( D1 = 1.432788 )
      PARAMETER      ( D2 = 0.189269 )
      PARAMETER      ( D3 = 0.001308 )

C LOCAL VARIABLES
      REAL            P, T, TT, TTT, A, B
      LOGICAL            INVERT

C BEGIN

C Algorithm works for ( 0.0 < p <= 0.5 )  hence
      IF (FRACTION_ABOVE_LEVEL .GT. 0.5 ) THEN
         P = 1.0 - FRACTION_ABOVE_LEVEL
         INVERT = .TRUE.
      ELSE
         P = FRACTION_ABOVE_LEVEL
         INVERT = .FALSE.
      END IF

      IF( P .LT. 1.0E-10 ) P = 1.0E-10

      T = SQRT( LOG( 1.0 / ( P * P ) ) )
      TT = T * T
      TTT = T * TT

      A = C0 + ( C1 * T ) + ( C2 * TT )
      B = 1.0 + ( D1 * T ) + ( D2 * TT ) + ( D3 * TTT )
      GAUSS_LEVEL = T - ( A / B )

      IF( INVERT ) GAUSS_LEVEL = -GAUSS_LEVEL

      RETURN

      END

      REAL FUNCTION TWOBIT_GAIN_R0( QN1, QZ1, QP1, QN2, QZ2, QP2 )
C----------------------------------------------------------------------
C
C      Finds gain of digital correlator for uncorrelated inputs 1,2
C      where the "gain paramaters" are ( qn1, qz1, qp1 ) and
C      ( qn2, qz2, qp2 ).  n=neg, z=zero, p=pos
C
C----------------------------------------------------------------------

C PARAMETERS
      REAL      QN1, QZ1, QP1, QN2, QZ2, QP2

C EXTERNAL FUNCTIONS
      REAL      G_QUAD

C BEGIN

      TWOBIT_GAIN_R0 = 0.159154943 *
     *            ( G_QUAD( QZ1, QP1, QZ2, QP2 ) +
     *                G_QUAD( QZ1, QN1, QZ2, QP2 ) +
     *                  G_QUAD( QZ1, QP1, QZ2, QN2 ) +
     *                    G_QUAD( QZ1, QN1, QZ2, QN2 ) )

      RETURN

      END



      REAL FUNCTION G_QUAD( QZ1, QP1, QZ2, QP2 )

      REAL QZ1, QP1, QZ2, QP2

      G_QUAD = 9 * QP1 * QP2 +
     *         3 * ( QZ1 * QP2 + QP1 * QZ2 ) + QZ1 * QZ2

      RETURN

      END

      SUBROUTINE UVWCAL (NBASE,IERR)
C-----------------------------------------------------------------------
C   Recompute U,V,W for a given baseline & integration.
C
C   Input (args):
C     NBASE    I      Number of baselines
C
C   Input (common):
C     LREC     I      Increment in UVBUFF for each baseline
C     UVBUFF   R      Buffer containing UV headers and data
C
C   Output (args):
C     IERR     I      Error return, 0 => success
C
C   Output (common):
C     UVBUFF   R      With modified u,v,w values
C
C   For each baseline, the 7 elements in UVBUFF are header values;
C     UVBUFF(x+1)     U (lambda)
C     UVBUFF(x+2)     V    "
C     UVBUFF(x+3)     W    "
C     UVBUFF(x+4)     Baseline (256*IA1+IA2)
C     UVBUFF(x+5)     Sample time
C     UVBUFF(x+6)     Source ID
C     UVBUFF(x+7)     Freq ID
C   where x = (baseline-1)*LREC2
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'

      INTEGER NBASE, IERR, I, IB, J, IA1, IA2, BAS_MAX, DMGOAD, IROUND
      INTEGER SOUID, ISRC, ISRC0, FREQID, IFNM
      DOUBLE PRECISION C, SPD, TAI0
      CHARACTER MSGTXT*80

      PARAMETER (C = 2.99792458D8)
      PARAMETER (SPD = 86400.0D0)
      PARAMETER (BAS_MAX = (ANT_MAX*(ANT_MAX-1))/2)
      PARAMETER (DMGOAD = 2*11*ANT_MAX)
      INTEGER  NXYUT, NCPOS, MJD0
      DOUBLE PRECISION XYUT(5,4), CPOS(3,4), PV(3,ANT_MAX), PHREF(3)
      DOUBLE PRECISION  TAI, INTGRN, SW(3,ANT_MAX)

C     INTEGER PLATE(ANT_MAX)
C     DOUBLE PRECISION EPOCH, PR, PD, APLX, RVEL, DPLX0,
C    *   GOAD(2,11,ANT_MAX), EPOCRD(ANT_MAX), TLAPSE(ANT_MAX),
C    *   TRPAUS(ANT_MAX), TAI, INTGRN, RAP, DAP, TE(3,ANT_MAX),
C    *   DELAY(ANT_MAX), DEL1D(ANT_MAX), DEL2D(ANT_MAX), VEL,
C    *   B(3,BAS_MAX), PARLCT(ANT_MAX), SW(3,ANT_MAX), SW1D(3,ANT_MAX)

      COMMON /ATECOM/ XYUT, CPOS, NXYUT, NCPOS, PV, PHREF, MJD0
C
C     DATA EPOCH, PR, PD, APLX, RVEL, DPLX0 / 6*0.0D0 /
C     DATA GOAD / DMGOAD*0.0D0/

C     DATA PLATE / ANT_MAX*7 /
C     DATA EPOCRD / ANT_MAX*47000.0D0 /
C     DATA TLAPSE / ANT_MAX*-6.0D0 /
C     DATA TRPAUS / ANT_MAX*13.0D0 /

C-----------------------------------------------------------------------
C  Sensible guesses needed to make EPHMRS work properly.
      DO I = 1, NANT
         IF (RP_PRESSURE(I).LE.0D0) RP_PRESSURE(I) = 950D0
         IF (RP_TEMP(I).LE.0D0) RP_TEMP(I) = 280D0
         IF (RP_HUMID(I).LE.0D0) RP_HUMID(I) = 30D0
      END DO

      IF (BASELN.EQ.-1) THEN
         WRITE(MSGTXT,105)
 105     FORMAT('UVWCAL: SYSCAL record out of order - skipping')
         CALL ATMSG(MSGTXT)
         GOTO 999
         END IF

      INTGRN = INTBASE

      DO I = 1, NBASE
         IB = (I-1)*LREC2
         IA2 = IROUND(UVBUFF(IB+4))
         IA1 = IA2/256
         IA2 = IA2 - IA1*256

         IF (IA1.GT.NANT .OR. IA2.GT.NANT) THEN
            WRITE(MSGTXT,110) UVBUFF(IB+4), NANT
 110        FORMAT ('UVWCAL: illegal baseline',f9.2,' NANT=',I5)
            CALL ATMSG(MSGTXT)
            IERR=-101
            GOTO 999
         END IF

         IF (NNSU.EQ.1) THEN
            ISRC = 1
         ELSE IF (NNSU.GT.1) THEN
            IFNM = 0
            ISRC = 0
            FREQID = IROUND(UVBUFF(IB+7))
            SOUID = IROUND(UVBUFF(IB+6))
            DO J = 1, NNIF
               IF (FQTAGS(J).EQ.FREQID) IFNM = J
            END DO
            IF (IFNM.GT.0 .AND. IFNM.LE.NNIF) THEN
               DO J = 1, NNSU
                  IF (SUTAGS(J,IFNM).EQ.SOUID) ISRC = J
               END DO
            END IF
         END IF

         IF (NNSU*NNIF*ISRC.LE.0 .OR. ISRC.GT.NNSU .OR. ISRC.LE.0) THEN
            WRITE(MSGTXT,120)
 120        FORMAT('UVWCAL: index error - talk to JER')
            CALL ATMSG(MSGTXT)
            IERR = -102
            GOTO 999
         END IF

         TAI = MJD0 + DBLE(UVBUFF(IB+5)) -
     *         (INTGRN/2.0D0 + RP_UTCMTAI)/SPD

C     Call the ephemeris unless inputs unchanged.
         IF (I.EQ.1 .OR. ISRC.NE.ISRC0 .OR. TAI.NE.TAI0) THEN

C            CALL EPHMRS (RP_UTCMTAI, RP_C, RP_DJMREFP, RP_DJMREFT,
C     *        XYUT, NXYUT, CPOS, NCPOS, SU_RA(ISRC), SU_DEC(ISRC),
C     *        EPOCH, PR, PD, APLX, RVEL, DPLX0, NANT, PV, GOAD, PLATE,
C     *        EPOCRD, RP_PRESSURE, RP_TEMP, RP_HUMID, TLAPSE, TRPAUS,
C     *        AXIS_OFFSET, PHREF, TAI, INTGRN, RP_DEFEAT, RAP, DAP,
C     *        TE, DELAY, DEL1D,  DEL2D, VEL, B, PARLCT, SW, SW1D)
C            ISRC0=ISRC
C            TAI0=TAI
            WRITE(MSGTXT,125)
 125        FORMAT('ATLOD: EPHMRS disabled - contact ATNF')
            CALL ATMSG(MSGTXT)
            IERR = -106
            GOTO 999

         END IF
C--Debugging
C         IF(SW1D(1,1)*SW1D(2,1)*SW1D(3,1).EQ.0D0) THEN
C            WRITE(MSGTXT,'(A,F18.10)') 'UVWCAL: sw1d zero at ',TAI
C            CALL ATMSG(MSGTXT)
C         END IF
         DO J=1,3
           UVBUFF(IB+J) = REFREQ/C * ( SW(J,IA1) - SW(J,IA2) )
         END DO
      END DO

 999  RETURN
      END


      SUBROUTINE ATEINI (TLUN, TFIND, IERR)
C-----------------------------------------------------------------------
C     Set up earth-orientation parameters for UVW recomputation.
C     Look for EOPC04 file in the EOP area and extract the data
C     appropriately. If not, use defaults of zero.
C
C   Output:
C     TLUN     I      Luns for files
C     TFIND    I      Pointers to FTAB
C     IERR     I      0 => file found and read OK.
C
C-----------------------------------------------------------------------
      INTEGER TLUN(*), TFIND(*), IERR
C
      INCLUDE 'RPFITS.INC'
      INCLUDE 'ATLOD.INC'
      INCLUDE 'INCS:DMSG.INC'

      INTEGER  ITRIM, IL, IT(6), MJD, I, K, IOS, IYR
      REAL     BUFF(6)
      DOUBLE PRECISION JDAY
      CHARACTER SYSFNAM*20, LINE*132, DATTMP*12
      LOGICAL EOPFND

      INTEGER  NXYUT, NCPOS, MJD0
      DOUBLE PRECISION XYUT(5,4), CPOS(3,4), PV(3,ANT_MAX), PHREF(3)
      COMMON /ATECOM/ XYUT, CPOS, NXYUT, NCPOS, PV, PHREF, MJD0

C-----------------------------------------------------------------------
C     Decide if our services are required. If dparm(8)=0, then
C     check for LBA data correlated before 20/02/1998.
      REDOUV = .FALSE.
      IF (RCMPUV.GT.0) THEN
         REDOUV=.TRUE.
      ELSE IF (RCMPUV.EQ.0) THEN
         IF (INSTRUMENT(1:5).EQ.'ATLBA') THEN
            DATTMP = DATWRIT
            CALL DATFST('F2L',DATTMP)
            DATWR8 = DATTMP
            CALL JULDAY (DATWR8, JDAY)
            REDOUV = (JDAY.LT.JULUVW)

            WRITE (MSGTXT, 166)
 166        FORMAT ('Recomputing UVWs for pre-20/02/1998 LBA data')
            CALL ATMSG (MSGTXT)

         END IF
      END IF

      IF (.NOT.REDOUV) RETURN

C     Fill in the antenna parameters
      DO I = 1, NANT
         PV(1,I) = X(I)
         PV(2,I) = Y(I)
         PV(3,I) = Z(I)
      END DO
      DO I=1,3
         PHREF(I) = PV(I,1)
      END DO

C     Defaults, in case no data found.
      NXYUT = 0
      NCPOS = 0

C     What year is it? Come to grips with it, as only AIPS can do.
      DATTMP = DATOBS
      CALL DATFST('F2L',DATTMP)
      DATOB8 = DATTMP
      CALL JULDAY (DATOB8, JDAY)
      CALL JD2DAT (JDAY, IT)
      MJD0 = INT(JDAY - 2400000.5D0)

      WRITE (SYSFNAM, '(A,I2.2)') 'EOP:EOPC04.', MOD(IT(1), 100)
      IL = ITRIM(SYSFNAM)
C
      EOPFND = .FALSE.
      CALL ZTXOPN ('READ', TLUN, TFIND, SYSFNAM, .FALSE., IERR)
      IF (IERR.EQ.0) THEN
         EOPFND = .TRUE.
      ELSE IF(RCMPUV.GT.0) THEN
         MSGTXT = 'ATEINI: EOP file not found: using all zeroes'
         CALL ATMSG (MSGTXT)
      END IF

C
      IF (EOPFND) THEN
C     First ensure the file if for the correct year.
         CALL ZTXIO ('READ', TLUN, TFIND, LINE, IERR)
         DO WHILE (IERR.EQ.0)
            IL = INDEX(LINE, '==>')
            IF (IL.GT.0) THEN
               READ(LINE(IL+3:),*, IOSTAT=IOS) IYR
               IF (IOS.EQ.0) THEN
                  IF (MOD(IYR, 100).EQ. MOD(IT(1), 100) ) THEN
                     GOTO 200
                  ELSE
                     IERR=-103
                     GOTO 900
                  END IF
               ELSE
                  IERR=-102
                  GOTO 900
               END IF
            END IF
            CALL ZTXIO ('READ', TLUN, TFIND, LINE, IERR)
         END DO
C     Fell through: no year found.
         IERR=-105
         GOTO 900

C     Scan through the file until the entry is found for the day
C     before the scan header date.

 200     MJD = 0
         DO WHILE(IERR.EQ.0 .AND. MJD.LT.MJD0)
            CALL ZTXIO ('READ', TLUN, TFIND, LINE, IERR)
            READ(LINE(10:),*,IOSTAT=IOS) MJD
         END DO

C     Read in up to four entries, beginning with that for the
C     day in question.

         K=0
         DO WHILE(IERR.EQ.0 .AND. K.LT.4)
            READ(LINE(10:),*,IOSTAT=IOS) MJD, (BUFF(I), I=1,6)
            IF (IOS.EQ.0 .AND. MJD.GE.MJD0) THEN
               K = K + 1
               XYUT(1,K) = MJD
C     The actual value used here is not critical.
               IF (RP_UTCMTAI.EQ.0D0) RP_UTCMTAI = -32D0  !1997/8 value
               XYUT(5,K) = -RP_UTCMTAI

               DO I=1,3
                  XYUT(I+1,K) = BUFF(I)
               END DO
               CPOS(1,K) = MJD
               CPOS(2,K) = BUFF(5)
               CPOS(3,K) = BUFF(6)
            END IF
            CALL ZTXIO ('READ', TLUN, TFIND, LINE, IERR)
         END DO

         NXYUT = K
         NCPOS = K
         IF (K.LT.2) THEN
            IERR=-104
            GOTO 900
         END IF
         CALL ZTXCLS (TLUN, TFIND, IERR)
         DO K=1,2
           WRITE (MSGTXT,210) (XYUT(I,K),I=1,5)
 210       FORMAT ('ATEINI: For MJD',F8.1,'  X,Y,dUT1 =',3F8.4,F5.1)
           CALL ATMSG (MSGTXT)
         END DO
      END IF

      RETURN

900   WRITE(MSGTXT, 910) IERR
910   FORMAT('EOPINI: problem with EOP file, IERR =',I5)
      CALL ATMSG (MSGTXT)

      RETURN
      END
      SUBROUTINE DATFIT(OLDDAT, NEWDAT, STATUS)

C To convert date from old form   dd/mm/yy
C                 to the new form yyyy/mm/dd
C or current date to new form.
C
C   call datfit(olddat, newdat, status)
C   olddat  character*8  input
C   newdat  character*12 returned
C   status  integer      returned
C
C on input:
C   if olddat is blank (' '), it uses current date.
C   otherwise interprets as old form of date dd/mm/yy
C returns status:
C   -1 if olddat is not a legal date in old format,
C    0 if date returned is current date
C    1 if date returned is olddat translated successfully

      integer ios, i, imon, iday, iyear2, status
      character*8 olddat, time
      character*12 newdat
      character*2 day, mon, year2
      character*4 year4
      character*2 nineteen
      character*1 day1, hyphen
      character*1 ctoday*24
      character*3 month(12), cmon
      character dayweek*3
      logical bad
      data month /'Jan','Feb','Mar','Apr','May','Jun','Jul',
     *   'Aug','Sep','Oct','Nov','Dec'/
      data nineteen /'19'/
      data hyphen/'-'/

      status = -1
      if ( olddat .ne. ' ') then
         read (olddat, '(i2,1x,i2,1x,i2)',iostat=ios) iday, imon, iyear2
         if (ios .eq. 0 ) then
            bad = .false.
            if (iday .lt.1 .or. iday .gt. 31) bad = .true.
            if (imon .lt.1 .or. imon .gt. 12) bad = .true.
            if (iyear2 .lt. 70) bad = .true.
         endif
         if (.not. bad) then
            read (olddat, '(a2,1x,a2,1x,a2)',iostat=ios) day, mon, year2
            if (ios .ne. 0 ) then
               bad = .true.
            else
               write (year4, '(a2,a2)') nineteen, year2
               status = 1
            end if
         end if
      else
C        first get current date as in 'Mon Aug 1 22:01:30 2001'
C        or                           'Mon Aug 11 22:01:30 2001'
         call fdate(ctoday)
         if (ctoday(10:10) .eq. ' ') then
            read (ctoday, '(a3,1x,a3,1x,a1,1x,a8,1x,a4)',iostat=ios)
     *         dayweek, cmon,day1,time,year4
            if (ios .lt.0) then
               bad = .true.
            else
               write (day,101) day1
101            format ('0',a1)
            end if
         else
            read (ctoday, '(a3,1x,a3,1x,a2,1x,a8,1x,a4)',iostat=ios)
     *         dayweek, cmon,day,time,year4
            if (ios .lt.0) then
               bad = .true.
            end if
         endif
         do i = 1,12
            if (.not.bad) then
               if (cmon .eq. month(i)) then
                  imon = i
                  write (mon, '(i2)',iostat=ios) imon
                  if (ios .lt.0) then
                     bad = .true.
                  end if
               endif
            endif
         enddo

         if (.not. bad) status = 0
      endif
      if (status .ge. 0) then
         if (day(1:1) .eq. ' ') day(1:1) = '0'
         if (mon(1:1) .eq. ' ') mon(1:1) = '0'
         write (newdat, '(a4,a1,a2,a1,a2,''  '')')
     *   year4, hyphen, mon, hyphen, day
      else
         newdat = ' '
      end if

      end
C=======================================================================
C   These are a set of byte-oriented routines to facilitate conversion
C   of data between different architectures.  They are designed to work
C   on any architecture which uses two's complement byte arithmetic.
C
C     IB2B: Big-endian integer -> big-endian integer (copy).
C     IB2L: big-endian integer -> little-endian integer (byte swap).
C     IB2V: big-endian integer -> VAX integer (byte swap).
C
C     IL2B: little-endian integer -> big-endian integer (byte swap).
C     IL2L: little-endian integer -> little-endian integer (copy).
C     IL2V: little-endian integer -> VAX integer (copy).
C
C     IV2B: VAX integer -> big-endian integer (byte swap).
C     IV2L: VAX integer -> little-endian integer (copy).
C     IV2V: VAX integer -> VAX integer (copy).
C
C     RB2B: IEEE big-endian real -> IEEE big-endian real (copy).
C     RB2L: IEEE big-endian real -> IEEE little-endian real (byte swap).
C     RB2V: IEEE big-endian real -> VAX real (format translation).
C
C     RL2B: IEEE little-endian real -> IEEE big-endian real (byte swap).
C     RL2L: IEEE little-endian real -> IEEE little-endian real (copy).
C
C     RV2B: VAX real -> IEEE big-endian real (format translation).
C     RV2L: VAX real -> IEEE little-endian real (format translation).
C     RV2V: VAX real -> VAX real (copy).
C
C   The routines take two BYTE(4) dummy arguments, the first being the
C   given value-to-convert and the second being the returned, converted
C   value.  They may be called with INTEGER or REAL actual arguments in
C   place of the BYTE(4) dummy arguments, and the arguments may have the
C   same address, e.g. CALL IV2B (B, B) is allowed.
C
C   Original: 1997/08/20 MRC
C=======================================================================
C   Two's complement integer representation
C-----------------------------------------------------------------------
C
C   The one's complement of a bit pattern is formed by interchanging
C   zeros and ones.  For example:
C
C           Binary pattern: 00110010
C         One's complement: 11001101
C
C   In two's complement arithmetic, negative integers are represented
C   as the one's complement plus 1.  For a one-byte signed integer
C
C                Binary  1: 00000001
C      Two's complement -1: 11111111
C
C   When a binary integer is added to its two's complement the result
C   is all zeros (binary 0) with the carry bit set (ignored).
C
C   Note the special case:
C
C           Binary pattern: 10000000
C         Two's complement: 10000000
C
C   This is taken to be the least negative integer, so the range for
C   one-byte signed integers is -128 to +127.  Similarly for multi-byte
C   integers.
C
C   All modern computers (including VAXs) use two's complement integer
C   representation.  For four-byte integers
C
C      byte:    0        1        2        3
C            siiiiiii iiiiiiii iiiiiiii iiiiiiii
C            ^                                 ^
C      bit: 31                                 0
C
C   These byte and bit numbers DO NOT reflect the order in memory, see
C   below.
C
C   Bit 31 is the sign bit and significance increases with increasing
C   bit number.
C
C   The "storage format" is commonly "big-endian" or "little-endian":
C
C                       Byte addresses in memory
C   Storage format     b      b+1     b+2     b+3
C   --------------    -----------------------------
C      Big-endian      0       1       2       3
C   Little-endian      3       2       1       0
C
C   Where b is the byte address in memory (b precedes b+1).  DEC (VAX,
C   Alpha) and INTEL machines (PCs) are little-endian while most others
C   are big-endian.
C
C=======================================================================
C   IEEE 754-1985 single-precision floating representation
C-----------------------------------------------------------------------
C
C   The bit-pattern of an IEEE 754-1985 single-precision floating point
C   number is
C
C      byte:    0        1        2        3
C            seeeeeee efffffff ffffffff ffffffff
C            ^                                 ^
C      bit: 31                                 0
C
C   These byte and bit numbers DO NOT reflect the order in memory, see
C   below.
C
C      bit field  (significance increases with increasing bit number)
C      ---------
C           31     sign bit
C        23-30     exponent
C         0-22     fraction
C
C   The IEEE single precision representation is summarized as follows:
C
C      seeeeeee efffffff ffffffff ffffffff
C      -----------------------------------
C      00000000 00000000 00000000 00000000    +0
C      10000000 00000000 00000000 00000000    -0
C          0 < e < 255                        Normalized number
C          e = 0, f != 0                      Unnormalized number
C      01111111 10000000 00000000 00000000    +Infinity
C      11111111 10000000 00000000 00000000    -Infinity
C      s1111111 11ffffff ffffffff ffffffff    Quiet NaN
C      s1111111 10ffffff ffffffff ffffffff    Signalling NaN (f != 0)
C
C   The value of a normalized number is
C
C      (-1)**s * 2**(e-127) * 1.f
C
C   where the implicit fraction bit occurs to the left of the binary
C   radix point.
C
C   The value of an unnormalized number is
C
C      (-1)**s * 2**(-126) * 0.f
C
C   Unnormalized numbers allow for "progressive underflow".
C
C   The IEEE 754-1985 standard explicitly does not specify a "storage
C   format", byte ordering is commonly "big-endian" or "little-endian":
C
C                       Byte addresses in memory
C   Storage format     b      b+1     b+2     b+3
C   --------------    -----------------------------
C      Big-endian      0       1       2       3
C   Little-endian      3       2       1       0
C
C   Where b is the byte address in memory (b precedes b+1).  DEC Alphas,
C   and INTEL machines (PCs) are commonly little-endian, while most
C   other IEEE 754 machines are big-endian.
C
C=======================================================================
C   VAX F_floating single-precision floating representation
C-----------------------------------------------------------------------
C
C   VAX F_floating format does not follow the IEEE 754 standard.  The
C   bit pattern is
C
C      byte:    0        1        2        3
C            seeeeeee efffffff ffffffff ffffffff
C            ^                                 ^
C      bit: 31                                 0
C
C   These byte and bit numbers DO NOT reflect the order in memory, see
C   below.
C
C      bit field  (significance increases with increasing bit number)
C      ---------
C           31     sign bit
C        23-30     exponent
C         0-22     fraction
C
C   The VAX F_floating representation is summarized as follows:
C
C      seeeeeee efffffff ffffffff ffffffff
C      -----------------------------------
C      00000000 0fffffff ffffffff ffffffff    0 (unsigned)
C      10000000 0fffffff ffffffff ffffffff    reserved operand
C          0 < e <= 255                       Normal number
C
C   The value of a normal number is
C
C      (-1)**s * 2**(e-128) * 0.1f
C
C   where the implicit fraction bit occurs to the right of the binary
C   radix point.
C
C                       Byte addresses in memory
C   Storage format     b      b+1     b+2     b+3
C   --------------    -----------------------------
C   VAX F_floating     1       0       3       2
C
C   The address of a VAX F_floating datum is the address, b, of byte 1.
C
C-----------------------------------------------------------------------
C
C   Apart from byte ordering, normal IEEE 754 numbers differ from VAX
C   F_floating numbers by a factor of 4 because:
C
C      * IEEE exponents are excess 127 whereas VAX exponents are excess
C        128.
C
C      * The IEEE implicit fraction bit occurs to the left of the binary
C        radix point wheras the VAX implicit bit occurs to the right.
C
C   The multiplication or division by 4 can be accomplished by adding
C   or subtracting 1 from byte 0.
C
C=======================================================================



      SUBROUTINE BCOPY (B1, B2)
C-----------------------------------------------------------------------
C  Does byte copying 0123 <--> 0123.
C-----------------------------------------------------------------------
      BYTE      B1(0:3), B2(0:3)
C-----------------------------------------------------------------------
C     Copy big-endian integer --> big-endian integer.
      ENTRY IB2B (B1, B2)

C     Copy little-endian integer --> little-endian integer.
      ENTRY IL2L (B1, B2)

C     Copy VAX integer --> VAX integer.
      ENTRY IV2V (B1, B2)
C-----------------------------------------------------------------------
C     Copy VAX integer --> little-endian integer.
      ENTRY IV2L (B1, B2)

C     Copy little-endian integer --> VAX integer.
      ENTRY IL2V (B1, B2)
C-----------------------------------------------------------------------
C     Copy IEEE big-endian real --> IEEE big-endian real.
      ENTRY RB2B (B1, B2)

C     Copy IEEE little-endian real --> IEEE little-endian real.
      ENTRY RL2L (B1, B2)

C     Copy VAX real --> VAX real.
      ENTRY RV2V (B1, B2)
C-----------------------------------------------------------------------
      B2(0) = B1(0)
      B2(1) = B1(1)
      B2(2) = B1(2)
      B2(3) = B1(3)

      RETURN
      END



      SUBROUTINE BSWAP (B1, B2)
C-----------------------------------------------------------------------
C   Does byte swapping 0123 <--> 3210.
C-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
C-----------------------------------------------------------------------
C     Convert VAX integer --> big-endian integer.
      ENTRY IV2B (B1, B2)

C     Convert big-endian integer --> VAX integer.
      ENTRY IB2V (B1, B2)
C-----------------------------------------------------------------------
C     Convert little-endian integer --> big-endian integer.
      ENTRY IL2B (B1, B2)

C     Convert big-endian integer --> little-endian integer.
      ENTRY IB2L (B1, B2)
C-----------------------------------------------------------------------
C     Convert IEEE little-endian real --> IEEE big-endian real.
      ENTRY RL2B (B1, B2)

C     Convert IEEE big-endian real --> IEEE little-endian real.
      ENTRY RB2L (B1, B2)
C-----------------------------------------------------------------------
      B(0) = B1(3)
      B(1) = B1(2)
      B(2) = B1(1)
      B(3) = B1(0)

      B2(0) = B(0)
      B2(1) = B(1)
      B2(2) = B(2)
      B2(3) = B(3)

      RETURN
      END

      SUBROUTINE RV2B (B1, B2)
C-----------------------------------------------------------------------
C     Converts VAX real --> IEEE big-endian real.
C-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
C-----------------------------------------------------------------------
      IF (B1(1).EQ.-128 .AND. B1(0).GE.0) THEN
C        VAX reserved operand, set to NaN.
         B2(0) = -1
         B2(1) = -1
         B2(2) =  0
         B2(3) =  0

      ELSE IF (B1(0).LT.0 .AND. (B1(1).EQ.127 .OR. B1(1).EQ.-1)) THEN
C        Overflow, set to signed infinity.
         B2(0) = B1(1)
         B2(1) = -128
         B2(2) =  0
         B2(3) =  0

      ELSE IF (B1(1).EQ.0 .AND. B1(0).GE.0) THEN
C        VAX zero.
         B2(0) = 0
         B2(1) = 0
         B2(2) = 0
         B2(3) = 0

      ELSE
C        Exchange bytes.
         B(0) = B1(1)
         B(1) = B1(0)
         B(2) = B1(3)
         B(3) = B1(2)

C        Scale VAX to IEEE.
         B(0) = B(0) - 1

C        Copy out.
         B2(0) = B(0)
         B2(1) = B(1)
         B2(2) = B(2)
         B2(3) = B(3)
      END IF

      RETURN
      END

      SUBROUTINE RV2L (B1, B2)
C-----------------------------------------------------------------------
C     Converts VAX real --> IEEE little-endian real.
C-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
C-----------------------------------------------------------------------
      IF (B1(1).EQ.-128 .AND. B1(0).GE.0) THEN
C        VAX reserved operand, set to NaN.
         B2(3) = -1
         B2(2) = -1
         B2(1) =  0
         B2(0) =  0

      ELSE IF (B1(0).LT.0 .AND. (B1(1).EQ.127 .OR. B1(1).EQ.-1)) THEN
C        Overflow, set to signed infinity.
         B2(3) = B1(1)
         B2(2) = -128
         B2(1) =  0
         B2(0) =  0

      ELSE IF (B1(1).EQ.0 .AND. B1(0).GE.0) THEN
C        VAX zero.
         B2(3) = 0
         B2(2) = 0
         B2(1) = 0
         B2(0) = 0

      ELSE
C        Exchange bytes.
         B(3) = B1(1)
         B(2) = B1(0)
         B(1) = B1(3)
         B(0) = B1(2)

C        Scale VAX to IEEE.
         B(3) = B(3) - 1

C        Copy out.
         B2(3) = B(3)
         B2(2) = B(2)
         B2(1) = B(1)
         B2(0) = B(0)
      END IF

      RETURN
      END
C-----------------------------------------------------------------------
C
C                  THE PROLOGUE
C
C-----------------------------------------------------------------------
C
C     This file contains :
C
C     RPFITSIN       Routine for reading FITS files
C
C     For information on the use of this software, and on the RPFITS
C     format, see the file RPFITS.DEFN
C
C
C     Please let me know immediately of any bugs.
C
C               Ray Norris
C              30 May 1985
C
C
C     MODIFICATIONS:
C     Modified RPN 29/9/88: major changes for IF axis and tables
C
C     rpn 9/11/88  major change in treatment of if's. For multi-IF data,
C                rpfits should be called once per IF (i.e. several
C                times per integration), with a formal parameter if_no
C                varying from 1 to NNIF.
C                A new group will be written for each IF, and needn't
C                be the same length. THUS GRPLENGTH CAN NOW VARY FROM
C                GROUP TO GROUP
C                PTI data will continue to be written with nstok = 2
C                Also added parameters FLAG, BIN, IF_NO, and SOURCENO
C                and FG and SU tables
C     rpn 8/2/89:  changed dates from AEST to UT
C     rpn 17/2/89: major mods: (1) changed to use FORTRAN BLK routines
C                          (2) changed record length from 512 to 2560,
C                              but included common RECL  so
C                              that old data can still be read.
C                          (3) Put IN_ and OUT_RECNO and RP_IOSTAT in
C                              INDEX common, and got rid of
C                              OLD_RPFITSIN
C     rpn 8/5/89:  Allow use of either blk_read or AT_READ by editing
C                the logical USE_BLK in the include
C                file RPFITS_SEL.INC
C     hm 9/5/89    Call routines (dummies on VAX) to translate
C                real and integer numbers from VAX format as they are
C                read from the RPFITS file, and before they are
C                written to the RPFITS file.
C     rpn 23/5/89  Fixed bugs in use of VAXI4, etc.
C     rpn 10/10/89 Allow FG table to be at the end of the data
C     rpn 10/10/89 Equivalence m(80) to buffer so FG table can be read
C                from data buffer (Yes this does work on the VAX)
C     rpn 20/3/90  Write IF tabel even if NNIF=1
C                Don't bother writing ANTENNA, TEMP, etc. cards
C                Introduced n_complex
C     rpn 21/3/90  MAJOR MODS: introduced syscal data group into
C                RPFITSIN and RPFITSOUT
C     hm 10/5/90   Split routines into separate files and made
C                mods necessary for compilation on SUNs.
C     hm 21/5/90   Added write_wt tests.
C     hm 28/5/90   Eliminated need for rpfits_sel.inc
C     hm 19/6/90   End of scan check changed.
C     hm 9/8/90    Recover from illegal randon parameters, possibly
C                caused by missing blocks.
C     rpn 16/11/90 Changed SIMPLE test to work on buffer instead of m
C     rpn 17/11/90 Tidied up code by using routine getparm
C     rpn 17/11/90 Tidied up group synch tests
C     hm  13/12/90 Made OK for AIPS:
C                 . no tabs in col 1
C                 . no code past col 72
C                 . 'C' not 'c' for comments
C     JER 04/01/91 GETPARM was being called with 4 more actual args
C                than formal args. Remove SC_UT, SC_ANT, SC_IF and
C                SC_Q from calling arg list: they're in common anyway.
C     JER 04/01/91 Initialise IF_CSTOK to blanks if no IF table found.
C     HM  03/02/91 Add more checks for bad data to illbase.
C     HM  15/11/91 Initialize new new IF entries if missing.
C     NEBK
C         08/01/92 Reworked conversion of floating point buffer value
C                  into integer baseline to avoid problems with
C                  arithmetic exceptions on wildly corrupt values
C                  CHanges in GETPARM, SKIPTHRU and ILLBASE for this
C     HM  19/02/92 For SYSCAL data - Put source number into sc_srcno .
C                  Note that for syscal, it is not returned as argument
C                  sourceno.
C     HM  23/06/93 Subroutine names to upper case and eliminate
C                  unused variables to stop compiler complaints.
C     HM  08/12/93 Added intbase - 10th random parameter
C     HM  10/03/94 Added proper motion keywords, PMRA, PMDEC, PMEPOCH
C     HM  10/01/95 Use AT_SKIP_EOF to speed up skipping to EOF on tape.
C     HM  28/2/96  Multi-beam data -
C                  introduce data_format=1 for real data, no weights.
C                  Also use data_format=2 for complex data no weights,
C                           data_format=3 for complex with weights.
C     HM  17/1/97  New random parameter,to override value of data_format
C             in header.
C     HM  14/2/97  use ant_max parameter in illbase.
C     HM  11/6/97  another change to bmax in illbase.
C     HM  07/8/97  force i_grphdr(grpptr+9) to real for VAXR4
C     HM  1998/06/30 Allow for version 1.0 or 2.0 rpfits. Change to date
C                    format for Year2000 compatibility.
C     HM  1998/11/03 Version 2.1 rpfits. Version now char*20.
C-----------------------------------------------------------------------
C
C                   SUBROUTINE RPFITSIN
C
C-----------------------------------------------------------------------
      SUBROUTINE RPFITSIN (jstat,vis,weight,baseline,ut,u,v,w,
     +   flag, bin, if_no, sourceno)
C-----------------------------------------------------------------------
C          Programmer: Ray Norris
C
C     Date: 25 April 1985
C
C
C-----------------DUMMY ARGUMENTS---------------------------------------

      integer baseline, flag, bin, if_no, sourceno, n_words
      real    weight(*), ut, u, v, w
      complex vis(*)

C--------------------OTHER BITS & PIECES--------------------------------

      INCLUDE 'RPFITS.INC'

      logical   endhdr, new_antenna, open, open_only, starthdr, endscan
      integer   AT_CLOSE, AT_OPEN_READ, AT_READ, AT_UNREAD, jstat,
     +   grplength, bufptr, bufleft, bufleft3, AT_SKIP_EOF,
     +   i_buff(640), grpptr, lun, i, j, k, ichar, nchar,
     +   pcount, i_grphdr(11), SIMPLE, icard, VAXI4, illegal, jc, jp
      real      grphdr(11), buffer(640), revis, velref,
     +   sc_buf(max_sc*max_if*ant_max), crpix4, VAXR4,
     +   last_good_ut
      character m(32)*80
      character*8 olddat
      integer datstat
      equivalence (i_buff(1), buffer(1))
      equivalence (i_grphdr(1), grphdr(1))
      equivalence (sc_buf(1), sc_cal(1,1,1))
      data illegal /32768/
      data open /.false./
      data new_antenna /.false./
      data last_good_ut/0/

      save
C------------------------DECIDE ON ACTION-------------------------------
      open_only = .false.

      if (jstat.eq.-3) go to 950
      if (jstat.eq.-2) go to 1000
      if (jstat.eq.-1) go to 2000
      if (jstat.eq.0) go to 3000
      if (jstat.eq.1) go to 5000
      if (jstat.eq.2) go to 6000
      write (6, *) ' Error in READFITS: illegal value of jstat=',jstat
      jstat = -1
      RETURN

C------------------------OPEN FITS FILE --------------------------------

  950 open_only = .true.
 1000 if (open) then
         write (6, *) ' File is already open'
      jstat = -1
      RETURN
      end if
      jstat = 0
      rp_iostat =  AT_OPEN_READ (aifile, lun, INTAPE)
      if (rp_iostat.ne.0) then
         jstat = -1
         write (6, *) ' Cannot open file'
         RETURN
      end if
      open = .true.
      if (open_only) RETURN

C----------------READ IN HEADER-----------------------------------------

 2000 endhdr = .false.
      starthdr = .false.
      bufptr = 0
      NNIF = 0
      icard = 1
      if (ncard.lt.0) ncard = -1
      an_found = .false.
      if_found = .false.
      su_found = .false.
      fg_found = .false.
      nx_found = .false.
      mt_found = .false.
      cu_found = .false.
      last_good_ut = 0.

C     Look for start of next header
      do while (.not.starthdr)
         rp_iostat = AT_READ (lun, i_buff)

         if(rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if
            write (6, *) ' RPFITSIN: Unable to read header block'
            write (6, *) ' RPFITSIN: rp_iostat = ',rp_iostat
            jstat = -1
            RETURN
         end if
c        write (m,'(32(20a4,:,/))') (buffer(j), j=1,640)
         JP = 1
         DO 10 JC = 1,32
            CALL H2CHR (80, JP, BUFFER, M(JC))
            JP = JP + 80
 10         CONTINUE
         if (m(1)(1:8).eq.'SIMPLE') starthdr = .true.
         if (m(1)(1:8).eq.'TABLE FG') then
            call RPFITS_READ_TABLE (lun, m, -1, endhdr)
            jstat = 4
            RETURN
         end if
      end do

C     Scan through header, getting the interesting bits
      do 2400 while (.not.endhdr)
         if (.not.starthdr) then
            rp_iostat = AT_READ (lun, i_buff)
C           write (m,'(32(20a4,:,/))') (buffer(j), j=1,640)
            JP = 1
            DO 20 JC = 1,32
               CALL H2CHR (80, JP, BUFFER, M(JC))
               JP = JP + 80
 20            CONTINUE
            if (rp_iostat.ne.0) then
               if (rp_iostat.eq.-1) then
                  jstat = 3
                  RETURN
               end if
               write (6, *) ' Unable to read header block'
               jstat = -1
               RETURN
            end if
         end if
         starthdr = .false.
         version = ' '
         do 2200 i = 1, 32
            if (m(i)(1:8).EQ.'VERSION ') then
               read (m(i)(12:31),'(a20)') version
            else if (m(i)(1:8).EQ.'RPFITS  ') then
               read (m(i)(12:31),'(a20)') rpfitsversion
            else if(m(i)(1:8).EQ.'NAXIS2') then
               read (m(i)(11:30),'(i20)')data_format
            else if (m(i)(1:8).EQ.'NAXIS3') then
               read (m(i)(11:30),'(i20)')nstok
            else if (m(i)(1:8).EQ.'NAXIS4') then
               read (m(i)(11:30),'(i20)')nfreq
            else if (m(i)(1:8).EQ.'NAXIS7') then
C              Note fudge for intermediate format PTI data
               read (m(i)(11:30),'(i20)')nstok
            else if (m(i)(1:8).EQ.'GCOUNT') then
               read (m(i)(11:30),'(i20)')ncount
            else if (m(i)(1:8).EQ.'PCOUNT') then
               read (m(i)(11:30),'(i20)')pcount
            else if (m(i)(1:8).EQ.'SCANS ') then
               read (m(i)(11:30),'(i20)')nscan
            else if (m(i)(1:8).EQ.'INTIME') then
               read (m(i)(11:30),'(i20)')intime
            else if (m(i)(1:8).EQ.'CRPIX4') then
               read (m(i)(11:30),'(g20.12)')crpix4
            else if (m(i)(1:8).EQ.'CRVAL4') then
               read (m(i)(11:30),'(g20.12)')freq
            else if (m(i)(1:8).EQ.'CDELT4') then
               read (m(i)(11:30),'(g20.12)')dfreq
            else if (m(i)(1:8).EQ.'CRVAL5') then
               read (m(i)(11:30),'(g20.12)')ra
            else if (m(i)(1:8).EQ.'CRVAL6') then
               read (m(i)(11:30),'(g20.12)')dec
            else if (m(i)(1:8).EQ.'RESTFREQ') then
               read (m(i)(11:30),'(g20.12)')rfreq
            else if (m(i)(1:8).EQ.'VELREF  ') then
               read (m(i)(11:30),'(g20.12)')velref
            else if (m(i)(1:8).EQ.'ALTRVAL ') then
               read (m(i)(11:30),'(g20.12)')vel1
            else if (m(i)(1:8).EQ.'OBJECT  ') then
               read (m(i)(12:30),'(a16)')object
            else if (m(i)(1:8).EQ.'INSTRUME') then
               read (m(i)(12:30),'(a16)')instrument
            else if (m(i)(1:8).EQ.'CAL     ') then
               read (m(i)(12:30),'(a16)')cal
            else if (m(i)(1:8).EQ.'OBSERVER') then
               read (m(i)(12:30),'(a16)') rp_observer
            else if (m(i)(1:8).EQ.'DATE-OBS') then
               read (m(i)(12:40),'(a12,12x,a4)') datobs, datsys
C              Version 1.0 had UT dates in the form dd-mm-yy
C              Version 2.0 has UT dates in the form yyy-mm-dd
               if (datobs(5:5) .ne. '-') then
                 read (datobs,'(a8,2x)') olddat
                 call datfit(olddat, datobs, datstat)
               end if
            else if (m(i)(1:8).EQ.'DATE    ') then
               read (m(i)(12:30),'(a12)') datwrit
C              Version 1.0 had UT dates in the form dd/mm/yy
               if (datwrit(5:5) .ne. '-') then
                 read (datwrit,'(a8,2x)') olddat
                 call datfit(olddat, datwrit, datstat)
               end if
            else if (m(i)(1:8).EQ.'EPOCH') then
               read (m(i)(12:30),'(a8)')coord
            else if (m(i)(1:5).EQ.'PRESS') then
               read (m(i)(6:40),'(i2,4x,g20.12)') k, rp_pressure(k)
            else if (m(i)(1:5).EQ.'TEMPE') then
               read (m(i)(6:40),'(i2,4x,g20.12)') k, rp_temp(k)
            else if (m(i)(1:5).EQ.'HUMID') then
               read (m(i)(6:40),'(i2,4x,g20.12)') k, rp_humid(k)
            else if (m(i)(1:5).EQ.'EPHEM') then
               read (m(i)(6:40),'(i2,4x,g20.12)') k, rp_c(k)
            else if (m(i)(1:8).EQ.'DEFEAT  ') then
               read (m(i)(11:30),'(i20)') rp_defeat
            else if (m(i)(1:8).EQ.'UTCMTAI ') then
               read (m(i)(11:30),'(g20.12)') rp_utcmtai
            else if (m(i)(1:8).EQ.'DJMREFP ') then
               read (m(i)(11:30),'(g20.12)') rp_djmrefp
            else if (m(i)(1:8).EQ.'DJMREFT ') then
               read (m(i)(11:30),'(g20.12)') rp_djmreft
            else if (m(i)(1:8).EQ.'PMRA    ') then
               read (m(i)(11:30),'(g20.12)') pm_ra
            else if (m(i)(1:8).EQ.'PMDEC   ') then
               read (m(i)(11:30),'(g20.12)') pm_dec
            else if (m(i)(1:8).EQ.'PMEPOCH ') then
               read (m(i)(11:30),'(g20.12)') pm_epoch
            else if (m(i)(1:6).eq.'TABLE ') then
C              Sort out tables
               call RPFITS_READ_TABLE (lun, m, i, endhdr)
            else if (m(i)(1:8).eq.'END     ') then
C              END card.
               endhdr = .true.
            end if

C           Write into "cards" array if necessary
            if (ncard.gt.0) then
               do j = 1, ncard
               nchar = 0
               do ichar = 1, 12
                  if (card(j)(ichar:ichar).ne.' ') nchar = ichar
               end do
               if (m(i)(1:nchar).eq.card(j)(1:nchar)) card(j)=m(i)
            end do
         else if (ncard.lt.0) then
            if (icard.le.max_card .and. .not.endhdr) then
               card(-ncard) = m(i)
               icard = icard + 1
               ncard = ncard - 1
            end if
         end if


C        read antenna parameters (a) OLD FORMAT
         if(m(i)(1:8).eq.'ANTENNA:') then
            if (.not.new_antenna) then
               nant = 0
               new_antenna = .true.
            end if
            read (m(i)(12:71),900) k,x(k),y(k),z(k),sta(k)
 900        format(I1,4x,g13.6,' Y=',g13.6,' Z=',g13.6,' STA=',a3)
            nant = nant+1
         end if

C        Read antenna parameters (b) NEW FORMAT
         if (m(i)(1:8).eq.'ANTENNA ') then
            if (.not.new_antenna) then
               nant = 0
               new_antenna = .true.
            end if
            read (m(i)(11:80),910) k,sta(k),x(k),y(k),z(k)
 910        format(I1,1x,a3,3x, g17.10,3x,g17.10,3x,g17.10)
            nant = nant+1
         end if

         if (endhdr) go to 2400
2200     continue
2400  continue
      ncard = ABS(ncard)

C     Set up for reading data.
      if (data_format.eq.1) then
         write_wt = .false.
         n_words = 1
      else if (data_format.eq.2) then
         write_wt = .false.
         n_words = 2
      else if (data_format.eq.3) then
         write_wt = .true.
         n_words = 3
      else
         write (6,*) 'RPFITSIN: NAXIS2 in file must be 1,2,3'
         jstat = -1
         return
      end if
C         write (6,*) 'RPFITSIN: NAXIS2 in file is ',data_format

C     Insert default values into table commons if tables weren't found
      if (.not. if_found) then
         NNIF = 1
         if_freq(1) = freq
         if_invert(1) = 1
         if_bw(1) = nfreq*dfreq
         if_nfreq(1) = nfreq
         if_nstok(1) = nstok
         if_ref(1) = crpix4
         do i=1,4
            if_cstok(i,1) = ' '
         end do
         if_simul(1) = 1
         if_chain(1) = 1
      else
         freq = if_freq(1)
         nfreq = if_nfreq(1)
C                                            hm 18may90 added -1 below
         if (if_nfreq(1).gt.1) then
            dfreq = if_bw(1)/(if_nfreq(1) - 1)
         else
            dfreq = if_bw(1)/if_nfreq(1)
         end if
         nstok = if_nstok(1)
      end if
      if (.not. su_found) then
         NNSU = 1
         su_name(1) = object
         su_ra(1) = ra
         su_dec(1) = dec
      else
         object = su_name(1)
         ra = su_ra(1)
         dec = su_dec(1)
      end if

C     Tidy up
      NNIF = max(NNIF, 1)
      ivelref = velref + 0.5
      new_antenna = .false.
      bufptr = 0
      jstat = 0
      RETURN

C----------------------READ DATA GROUP HEADER --------------------------
3000  continue

C     THE FOLLOWING POINTERS AND COUNTERS ARE USED HERE:
C     GRPLENGTH      No. of visibilities in group
C     GRPPTR         Pointer to next visibility in group to be read
C     BUFPTR         Pointer to next word to be read in current buffer
C     BUFLEFT        No. of words still to be read from current buffer


C     Note that data are read in blocks of 5 records = 640 (4byte) words

      grpptr = 1
      if_no = 1


      if (bufptr.eq.0.or.bufptr.eq.641) then
         rp_iostat = AT_READ (lun, i_buff)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if

            jstat = -1
            write (6, *) ' Cannot read data'
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, i_buff)
            RETURN
         end if

         bufptr = 1

      end if


C     READ PARAMETERS FROM FITS FILE
C     FORMAT FROM RPFITS IS:
C      ------ VIS data -------------      ----------- SYSCAL data ----
C      (baseline > 0)                         (baseline = -1)
C      param 1=u in m                         0.0
C      param 2=v in m                         0.0
C      param 3=w in m                         0.0
C      param 4=baseline number                -1.0
C      param 5=UT in seconds                  sc_ut: UT in seconds
C      param 6= flag (if present)             sc_ant
C      param 7= bin  (if present)             sc_if
C      param 8=if_no (if present)             sc_q
C      param 9=sourceno (if present)          sc_srcno
C      param 10=intbase (if present)          intbase (if present)

3100  bufleft = 641 - bufptr

C          ---------check for end of scan -------------
C     This is indicated by buffer being padded out with reserved
C     operands.
C>
C      if (VAXI4(i_buff(bufptr)).eq.illegal) then
C=
C     Old rpfits files may be padded with zeros, so check for u,
C     baseline no and UT all zero. Assume that if next vis
C     incomplete at end of buffer, next buffer will be all zeros.
C                                               Mod by HM 19jun90
      endscan = .false.
      if (bufleft .ge. pcount) then
         if (VAXI4(i_buff(bufptr)).eq.0
     +      .and. VAXI4(i_buff(bufptr+3)).eq. 0
     +      .and. VAXI4(i_buff(bufptr+4)).eq. 0) then
            endscan = .true.
         end if
      end if

      if (VAXI4(i_buff(bufptr)).eq.illegal .or. endscan ) then
C<
         rp_iostat = AT_READ (lun, i_buff)
         if(rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if
            write (6, *) ' Unable to read header block'
            jstat = -1
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, i_buff)
            RETURN
         end if

         bufptr = 1
         jstat = 5
         RETURN
      end if

C     ------------NOW READ DATA -------------

      if (bufleft.ge.pcount) then

C        If it will all fit in current buffer, then things are easy
         call getparm (jstat, buffer, i_buff, bufptr, bufptr, buffer,
     +      i_buff, pcount, u, v, w, baseline, lun,
     +      ut, flag, bin, if_no, sourceno)
         if (jstat.eq.-2) goto 3100
         if (jstat.ne.0) return
         bufptr = bufptr+pcount

      else
C        We can recover only part of the group header.
C        dispose of what we have, then read the remainder from
C        the next batch of data (pcount blocks).

         do i = 1,bufleft
            grphdr(i) = buffer(bufptr+i-1)
         end do
         rp_iostat = AT_READ (lun, i_buff)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if
            jstat = -1
            write (6, *) ' Cannot read data'
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, i_buff)
            RETURN
         end if

         bufptr = pcount-bufleft

C        Extract bufptr items from the next buffer
         do i = 1, bufptr
            grphdr(i+bufleft) = buffer(i)
         end do

         call getparm (jstat, grphdr, i_grphdr, 1, bufptr, buffer,
     +      i_buff, pcount, u, v, w, baseline, lun,
     +      ut, flag, bin, if_no, sourceno)
         if (jstat.eq.-2) goto 3100
         if (jstat.ne.0) return

C        Set bufptr to the first visibility in the new buffer.
         bufptr = bufptr + 1

      end if


C     Determine GRPLENGTH
      if (baseline.eq.-1) then
         grplength = sc_q*sc_if*sc_ant
      else if (if_no.gt.1) then
         grplength = if_nfreq(if_no)*if_nstok(if_no)
      else
         grplength = nstok*nfreq
      end if

      if (baseline.eq.-1) go to 4000

C----------------------READ VIS DATA GROUP -----------------------------


C     READ DATA FROM FITS FILE, FORMAT FROM RPFITS IS:
C        data_format  3        2        1
C        word 1 =   Re(vis)   Re(vis)   Real(vis)
C        word 2 =   Imag(vis) Imag(vis) -
C        word 3 =   weight    -         -

3500  continue
C     Set up for reading data. data_format read from header.
      if (data_format.eq.1) then
         write_wt = .false.
         n_words = 1
      else if (data_format.eq.2) then
         write_wt = .false.
         n_words = 2
      else if (data_format.eq.3) then
         write_wt = .true.
         n_words = 3
      else
         write (6,*) 'RPFITSIN: NAXIS2 in file must be 1,2,3'
         jstat = -1
         return
      end if

      last_good_ut = ut

      bufleft = 641-bufptr
      if (bufleft.ge.(n_words*(grplength-grpptr+1))) then

C        If entire group can be filled from existing buffer then do so
         do i = grpptr, grplength
            if (data_format .eq. 1) then
               vis(i) = VAXR4(buffer(bufptr))
            else
               vis(i) = CMPLX(VAXR4(buffer(bufptr)),
     +                        VAXR4(buffer(bufptr+1)))
               if (write_wt) then
                  weight(i) = VAXR4(buffer(bufptr+2))
               end if
            end if
            bufptr = bufptr+n_words
         end do
         jstat = 0
         RETURN
      else
C        Otherwise things are a bit more complicated, first read
C        complete visibilities in old buffer.
         bufleft3 = bufleft/n_words
         do i = 1,bufleft3
            if (data_format .eq. 1) then
               vis(grpptr+i-1) = VAXR4(buffer(bufptr))
            else
               vis(grpptr+i-1) = CMPLX(VAXR4(buffer(bufptr)),
     +                                 VAXR4(buffer(bufptr+1)))
               if (data_format .eq. 3) then
                  weight(grpptr+i-1) = VAXR4(buffer(bufptr+2))
               end if
            end if
            bufptr = bufptr+n_words
         end do
         grpptr = grpptr+bufleft3

C        Read the fraction of a visibility left in old buffer
C        Should not happen for n_words=1
         bufleft = bufleft-n_words*bufleft3
         if (bufleft.eq.1) revis = VAXR4(buffer(640))
         if (n_words.eq.3 .and. bufleft.eq.2) vis(grpptr) =
     +      CMPLX(VAXR4(buffer(639)), VAXR4(buffer(640)))

C        Now read in a new buffer
         rp_iostat = AT_READ (lun, i_buff)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if
            jstat = -1
            write (6, *) ' Cannot read data'
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, i_buff)
            RETURN
         end if

C        Fill any incomplete visibility (n_words=2 or 3 only)
         if (bufleft.eq.0) then
            bufptr = 1
         else if (bufleft.eq.1) then
            vis(grpptr)    = CMPLX(revis,VAXR4(buffer(1)))
            if (write_wt) then
               weight(grpptr) = VAXR4(buffer(2))
            endif
            grpptr = grpptr+1
            bufptr = n_words
         else if (bufleft.eq.2 .and. n_words.eq.3) then
            if (write_wt) then
               weight(grpptr) = VAXR4(buffer(1))
            end if
            grpptr = grpptr+1
            bufptr = 2
         end if

C     Return to pick up the rest of the group
      end if
      go to 3500

C----------------------READ SYSCAL DATA GROUP --------------------------


C     READ DATA FROM FITS FILE
C     note that in this conmtext GRPLENGTH is in units of words,
C     not visibilities .
 4000 continue

      bufleft = 641-bufptr
      if (bufleft.ge.(grplength-grpptr+1)) then

C        If entire group can be filled from existing buffer then do so
         do i = grpptr, grplength
            sc_buf(i) = VAXR4(buffer(bufptr))
            bufptr = bufptr+1
         end do
         jstat = 0
         RETURN

      else

C        Otherwise read complete visibilities in old buffer
         do i = 1,bufleft
            sc_buf(grpptr+i-1) = VAXR4(buffer(bufptr))
            bufptr = bufptr+1
         end do
         grpptr = grpptr+bufleft

C        Then read in a new buffer
         rp_iostat = AT_READ (lun, i_buff)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat = 3
               RETURN
            end if
            jstat = -1
            write (6, *) ' Cannot read data'
            RETURN
         end if

         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, i_buff)
            RETURN
         end if
         bufptr = 1

C        and then return to pick up the rest of the group
      end if
      go to 4000

C----------------------CLOSE FITS FILE----------------------------------

5000  continue
      rp_iostat = AT_CLOSE (lun)
      if (rp_iostat.ne.0) then
         jstat = -1
         write (6, *) ' Cannot close file'
         RETURN
      end if
      jstat = 0
      open = .false.
      RETURN

C---------------- SKIP TO END OF FILE-----------------------------------

 6000 rp_iostat = AT_SKIP_EOF (lun)
      if (rp_iostat.eq.-1) then
         jstat = 3
      else
         write (6, *)
     +      ' unable to skip-to-EOF'
         jstat = -1
         RETURN
      end if

      end

      INTEGER FUNCTION SIMPLE (buffer, lun)
C-----------------------------------------------------------------------
C     SIMPLE tests for the start of a new header or FG (flag) table.
C
C-----------------------------------------------------------------------
      logical   endhdr
      integer   lun, j
      character m(80)*32
      real buffer(640)
C-----------------------------------------------------------------------
C     Assume not.
      SIMPLE = 0

C     write first 8 characters from buffer into character string m
      write (m(1)(1:8),'(2a4)') (buffer(j), j=1,2)

      if (m(1)(1:6).eq.'SIMPLE') then
C        Start of header.
         SIMPLE = 1

      else if (m(1)(1:8).eq.'FG TABLE') then
C        Start of FG (flag) table.
         endhdr = .false.
         write (m,'(32(20a4,:,/))') (buffer(j), j=1,640)
         call RPFITS_READ_TABLE (lun, m, -1, endhdr)
         SIMPLE = 4
      end if

      return
      end

      SUBROUTINE GETPARM (jstat, grphdr, i_grphdr, grpptr, bufptr,
     +buffer, i_buff, pcount, u, v, w, baseline, lun,
     +ut, flag, bin, if_no, sourceno)
C-----------------------------------------------------------------------
C     routine to read in the group header parameters from grphdr
C     and check for legality. If legal data is not found, then the data
C     is skipped until some legal data are found, and then the new
C     buffer and bufptr are returned.
C     rpn 17/11/90
C
C     jstat is 0 on exit for immediate success,  or -2 if success
C     was achieved after skipping data, or -1 for a total lack of
C     success
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'

      integer jstat, i_grphdr(640), bufptr, baseline, grpptr,
     +VAXI4, pcount,flag, bin, if_no, sourceno, lun, i_buff(640)
      real grphdr(640), buffer(640), u, v, w, ut, VAXR4, rbase
      logical illbase
C-----------------------------------------------------------------------
C     First 5 parameters are always there (you hope!)
      u = VAXR4(grphdr(grpptr))
      v = VAXR4(grphdr(grpptr+1))
      w = VAXR4(grphdr(grpptr+2))
      rbase = VAXR4(grphdr(grpptr+3))
      baseline = NINT(rbase)
      ut = VAXR4(grphdr(grpptr+4))

C     Now look for syscal parameters
      if (baseline.eq.-1) then
         sc_ut = ut
         sc_ant = VAXI4(i_grphdr(grpptr+5))
         sc_if = VAXI4(i_grphdr(grpptr+6))
         sc_q = VAXI4(i_grphdr(grpptr+7))
         sc_srcno = VAXI4(i_grphdr(grpptr+8))
         if (pcount .gt. 9) then
            intbase = VAXR4(real(i_grphdr(grpptr+9)))
         else
            intbase = 0.0
         end if

C     Else pick up remaining parameters
      else if (pcount.gt.5) then
         flag = VAXI4(i_grphdr(grpptr+5))
         bin = VAXI4(i_grphdr(grpptr+6))
         if_no = VAXI4(i_grphdr(grpptr+7))
         sourceno = VAXI4(i_grphdr( grpptr+8))
         if (pcount .gt. 9) then
            intbase = VAXR4(grphdr(grpptr+9))
         else
            intbase = intime
         end if
         if (pcount .gt. 10) then
            data_format = VAXI4(i_grphdr(grpptr+10))
C           If pcount is 10 or less, data_format comes from scan header
         end if


      end if

C     Check for illegal params.
      if (illbase(rbase, if_no, ut) )then
C        this can be caused by a bad block, so look for more data
         write (6, *) ' illegal data (or end of scan on older data)'
         call skipthru(jstat, bufptr, buffer, lun, i_buff)
         return
      end if
      jstat=0
      return
      end

C**********************************************************************

      SUBROUTINE SKIPTHRU (jstat, bufptr, buffer, lun, i_buff)
C-----------------------------------------------------------------------
C     routine to skip through data looking for recognisable data or
C     header.
C     rpn 17/11/90
C     jstat = -2 if successful
C     add i_buff equivalenced to buffer
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'

      integer jstat, bufptr, baseline, i, j, lun, at_read, at_unread,
     +simple, r_to_i, vaxi4, if_no, i_buff(640)
      real buffer(640), u,v,w, ut, vaxr4, rbase
      logical illbase
C-----------------------------------------------------------------------
      do 999 j=1,1000

C        First read a new block, since remains of old one is unlikely to
C        contain anything useful (and at most one integration)
         rp_iostat = AT_READ (lun, i_buff)
         if (rp_iostat.ne.0) then
            if (rp_iostat.eq.-1) then
               jstat=3
               return
            end if
            write (6,*) ' Unable to read next block'
            jstat=-1
            return
         end if

C        check to see if it's a header block
         jstat = SIMPLE (buffer, lun)
         if (jstat.ne.0) then
            rp_iostat = AT_UNREAD (lun, i_buff)
            RETURN
         end if
         bufptr=1

C        skip through the block looking for something legal
         do i=1,640

            u        = VAXR4(buffer(bufptr))
            v        = VAXR4(buffer(bufptr+1))
            w        = VAXR4(buffer(bufptr+2))
            rbase    = VAXR4(buffer(bufptr+3))
            ut       = VAXR4(buffer(bufptr+4))
            if_no    = VAXI4(r_to_i(buffer(bufptr+7)))

            if (.not. illbase(rbase, if_no, ut)) then
               baseline = NINT(rbase)
               goto 200
            end if

            bufptr=bufptr+1
            if (bufptr.gt.632) goto 999
         end do

C        ok lets do it all again
  999 continue

C     Success!
  200 jstat=-2
      return
      end

      INTEGER FUNCTION R_TO_I (X)
C-----------------------------------------------------------------------
C     Function to interpret a real as an integer
C     rpn 17/11/90
C-----------------------------------------------------------------------
      real x, y
      integer i
      equivalence (i,y)
C-----------------------------------------------------------------------
      y = x
      R_TO_I = i
      return
      end

      LOGICAL FUNCTION ILLBASE (baseline, if_no, ut)
C-----------------------------------------------------------------------
C     Function to check for a legal baseline and if_no
C     returns true for illegal and false for legal params
C
C     Use a floating point baseline to avoid arithmetic
C     exceptions trying to integerize what might be a wildly
C     corrupt data with NINT (problem encountered with
C     AT data in early Jan 1992
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'

      integer  if_no
      real     ut, baseline, bmax, bmin
      logical sysc
      parameter (bmin = 256 + 1)
C-----------------------------------------------------------------------
      bmax = nant * 256 + nant
C     Deal with floating point baseline first
C
      illbase = .false.
      sysc = (baseline.gt.-1.0001 .and. baseline.lt.-0.9999)
      if (.not.sysc) then
         if (baseline.gt.bmax .or. baseline.lt.bmin) then
            illbase = .true.
            return
         else
            if (abs(baseline - nint(baseline)).gt.0.001) then
C              This value is not close enough to an integer
C              to be valid
               illbase = .true.
               return
            end if
         end if
      end if

C     Now check the rest
      illbase = illbase .or. (
     +  (ut.lt.0. .or. ut.gt.172800.) .or.

     +  (.not.sysc .and.
     +  (if_no.lt.0 .or. if_no.gt.max_if)) .or.

     +  (sysc .and.
     +  ((sc_ant.lt.1 .or. sc_ant.gt.ant_max) .or.
     +  (sc_if.lt.1  .or. sc_if.gt.max_if) .or.
     +  (sc_q.lt.1   .or. sc_q.gt.100))))

      return
      end

      SUBROUTINE RPFITS_READ_TABLE(lun, m, ii, endhdr)
C-----------------------------------------------------------------------
C     routine to read any  FITS tables right up to
C     the end of the header
C     On entry:
C                lun is the lun of the RPFITS file
C                m is the array of card images
C                ii is the current line in the array m, or is set to -1
C                    if only the flag table (at the end of the data) is
C                    to be read
C                endhdr will be set to true if the end of header is
C                    encountered
C     RPN 29/9/88
C     HM  11/5/90  Made mods necessary for compilation on SUNs
C     HM  29/1/91  Reduced lines to 72 chars
C     HM  15/5/92  Allow for up to source number up to 999 in IF, FG, SU
C                  and CU tables. Also increased are if_simul in IF
C                  table and entry number in flag table.
C     HM  26/8/92  Fix bug. Set i=1 not 0 after write.
C     HM  23/6/93  Eliminate unused variables.
C     HM  11/3/94  Changed format of MT table (i4 to i5)
C-----------------------------------------------------------------------
      logical   endhdr, fg_only
      integer   lun, i, ii, status, AT_READ, ichr(640), j
      character m(32)*80
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'

      i = ABS(ii)
      fg_only = (ii.eq.-1)
      do while (.not. endhdr)

         if (ncard.lt.0) then
            card(-ncard) = m(i)
            ncard = ncard - 1
         end if

         if (m(i)(1:8).eq.'TABLE IF') then
            call READIF (lun, m, i)
         else if (m(i)(1:8).eq.'TABLE SU') then
            call READSU (lun, m, i)
         else if (m(i)(1:8).eq.'TABLE FG') then
            call READFG (lun, m, i)
         else if (m(i)(1:8).eq.'TABLE AN') then
            call READAN (lun, m, i)
         else if (m(i)(1:8).eq.'TABLE MT') then
            call READMT (lun, m, i)
         else if (m(i)(1:8).eq.'TABLE CU') then
            call READCU (lun, m, i)
         else if (m(i)(1:8).eq.'END     ') then
            endhdr = .true.
            return
         end if

         if (fg_only) then
            endhdr = .false.
            return
         end if

         i = i + 1
         if (i.gt.32) then
            status = AT_READ (lun, ichr)
            write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
            i = 1
         end if
      end do

      return
      end

      SUBROUTINE READIF (lun, m, i)
C-----------------------------------------------------------------------
C     routine to read an IF table from an RPFITS file
C
C     RPN 29/9/88
C     Modified 14/Dec/91 HM  Added if_simul and if_chain. Old data
C     without them is given if_simul=if_chain=1 in the IF table.
C     Modified 14/May/92 HM  Allow for i3 IFNUMS or if_simul.
C-----------------------------------------------------------------------
      integer   lun, i, j, k, l, status, AT_READ, ichr(640)
      character m(32)*80, temp*5
C-----------------------------------------------------------------------
      INCLUDE 'RPFITS.INC'

      NNIF = 0
      do while (.true.)
         do j = i + 1,32
            if (ncard.lt.0) then
               card(-ncard) = m(j)
               ncard = ncard - 1
            end if

            if (m(j)(1:8).eq.'ENDTABLE') then
               i = j
               goto 999
            else if (m(j)(1:8).eq.'HEADER') then
            else if (m(j)(1:8).eq.'COMMENT') then
            else
               k = NNIF + 1
               read(m(j),'(BN,i3,f16.3,1x,i2, 1x, f16.3, 1x, i4,
     +            1x, i2, 1x, 4a2, 1x,i1, 1x,f6.1, 1x, a5)')
     +            IFNUMS(k), if_freq(k), if_invert(k),
     +            if_bw(k), if_nfreq(k), if_nstok(k),
     +            (if_cstok(l,k),l = 1,4), if_sampl(k),
     +            if_ref(k), temp
               if (temp .eq. ' ') then
                  if_simul(k) = 1
                  if_chain(k) = 1
               else
                  read (temp,*) if_simul(k), if_chain(k)
                  if (if_simul(k) .eq. 0) if_simul(k) = 1
                  if (if_chain(k) .eq. 0) if_chain(k) = 1
               end if
               NNIF = NNIF + 1
            end if
         end do

         status = AT_READ (lun, ichr)
         write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
         i = 0
      end do

  999 if_found = .true.
      return
      end

      SUBROUTINE WRITE_IF_TABLE (i, m)
C-----------------------------------------------------------------------
C     routine to write an IF table to an RPFITS file
C
C     RPN 29/9/88
C     Modified 14/Dec/91 HM  Added if_simul and if_chain. Note that it
C                            are read with BZ to interpret the blanks in
C                            old data as zero.
C     Modified 14/May/92 HM  Write IFNUMS as i3.
C-----------------------------------------------------------------------
      integer   i, k, l
      character m(*)*80, header*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      header =
     +   'HEADER     FREQ    INVERT   BW         '//
     +   'NCHAN NSTOK TYPE SAM REF SIM CHAIN'
      i = i+1
      write (m(i),'(a)')  'TABLE IF'
      i = i+1
      write (m(i),'(a)')  header

      do k = 1, NNIF
         i = i+1
         write (m(i),'(i3,f16.3,1x,i2, 1x, f16.3, 1x, i4, 1x,
     +      i2 , 1x, 4a2,1x,i1, 1x, f6.1, 1x, i2, 1x, i2)')
     +      IFNUMS(k), if_freq(k), if_invert(k),
     +      if_bw(k), if_nfreq(k), if_nstok(k),
     +      (if_cstok(l,k),l=1,4), if_sampl(k),
     +      if_ref(k), if_simul(k), if_chain(k)
      end do

      i = i+1
      write (m(i),'(a)')  'ENDTABLE'

      return
      end

      SUBROUTINE READSU(lun, m, i)
C-----------------------------------------------------------------------
C     routine to read a SOURCE table from an RPFITS file
C
C     RPN 8/11/88
C     HM  15/5/92 Read old (i2) or new (i3) source number.
C-----------------------------------------------------------------------
      integer   lun, i, j, k, status, AT_READ, ichr(640)
      character m(32)*80, LCH*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      NNSU = 0
      do while (.true.)
         do j = i + 1,32
            LCH = M(J)
            if (ncard.lt.0) then
               card(-ncard) = m(j)
               ncard = ncard-1
            end if

            if (m(j)(1:8).eq.'ENDTABLE') then
               i = j
               goto 999
            else if (m(j)(1:8).eq.'HEADER') then
            else if (m(j)(1:8).eq.'COMMENT') then
            else
               k = NNSU+1
               read(m(j),'(BN,i3,a16,1x,f12.9, 1x, f12.9, 1x, a4,
     +            1x, f11.9, 1x, f11.9)')
     +            SUNUM(k), su_name(k), su_ra(k), su_dec(k),
     +            su_cal(k), su_rad(k), su_decd(k)
               NNSU = NNSU+1
            end if
         end do

         status = AT_READ (lun, ichr)
         write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
         i = 0
      end do

  999 su_found = .true.
      return
      end

      SUBROUTINE WRITE_SU_TABLE (i, m)
C-----------------------------------------------------------------------
C     routine to write a SOURCE table to an RPFITS file
C
C     RPN 8/11/88
C     HM 15/05/92 Modified to write SUNUM as i3.
C-----------------------------------------------------------------------
      integer i, k
      character m(*)*80, header*80
      INCLUDE 'RPFITS.INC'
      data header(1:40)/
     +   'HEADER  NAME            RA2000   DEC2000'/
      data header(41:80)/
     +   '  CALCODE  RA_DATE    DEC_DATE          '/
C-----------------------------------------------------------------------
      i = i+1
      write (m(i),'(a)')  'TABLE SU'
      i = i+1
      write (m(i),'(a)')  header

      do k = 1, NNSU
         i = i+1
         write (m(i),'(i3,a16,1x,f12.9, 1x, f12.9, 1x, a4,
     +      1x, f11.9, 1x, f11.9)')
     +      SUNUM(k), su_name(k), su_ra(k), su_dec(k), su_cal(k),
     +      su_rad(k), su_decd(k)
      end do

      i = i+1
      write (m(i),'(a)')  'ENDTABLE'

      return
      end

      SUBROUTINE READFG (lun, m, i)
C-----------------------------------------------------------------------
C     routine to read a FLAG table from an RPFITS file
C
C     RPN 8/11/88
C     Modified 15/5/92 HM Read old (i2) and new (i3) j and fg_if
C-----------------------------------------------------------------------
      integer lun, i, j, k, status, AT_READ, ichr(640)
      character m(32)*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      n_fg = 0
      do while (.true.)
         do k = i + 1,32
            if (ncard.lt.0) then
               card(-ncard) = m(k)
               ncard = ncard - 1
            end if
            if (m(k)(1:8).eq.'ENDTABLE') then
               i = k
               goto 999
            else if ( m(k)(1:8).eq.'HEADER' ) then
            else if ( m(k)(1:8).eq.'COMMENT') then
            else
               read(m(k),'(BN, i3, i2, 1x, i2, 2(1x,f8.1), 1x, 2(i3),
     +            i4, 1x, i4, 2(1x,i1), a24)') j,
     +            fg_ant(1,j), fg_ant(2,j), fg_ut(1,j), fg_ut(2,j),
     +            fg_if(1,j), fg_if(2,j), fg_chan(1,j), fg_chan(2,j),
     +            fg_stok(1,j), fg_stok(2,j), fg_reason
               n_fg = n_fg+1
            end if
         end do

         status = AT_READ (lun, ichr)
         write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
         i = 0
      end do

  999 fg_found = .true.
      return
      end

      SUBROUTINE WRITE_FG_TABLE (i, m)
C-----------------------------------------------------------------------
C     routine to write a FLAG table to an RPFITS file
C
C     RPN 8/11/88
C     Modified 15/5/92 HM Write j and fg_if as i3.
C-----------------------------------------------------------------------
      integer i, j
      character m(*)*80, header*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      header =
     +   'HEADER  ANT   UT    IF     CHAN     STOK       REASON'
      i = i + 1
      write (m(i),'(a)')  'TABLE FG'
      i = i + 1
      write (m(i),'(a)')  header

      do j = 1, n_fg
         i = i + 1
         write (m(i),'(i3, i2, 1x, i2, 2(1x,f8.1), 1x, i3, i3,
     +      i4, 1x, i4, 2(1x,i1), a24)') j,
     +      fg_ant(1,j), fg_ant(2,j), fg_ut(1,j), fg_ut(2,j),
     +      fg_if(1,j), fg_if(2,j), fg_chan(1,j), fg_chan(2,j),
     +      fg_stok(1,j), fg_stok(2,j), fg_reason(j)
      end do

      i = i + 1
      write (m(i),'(a)')  'ENDTABLE'

      return
      end

      SUBROUTINE READAN (lun, m, i)
C-----------------------------------------------------------------------
C     routine to read an AN table from an RPFITS file
C
C     RPN 27/7/89
C     mod rpn 11/10/89 remove met info
C     H.May 26/8/92  Change read to match write in write_an_table
C-----------------------------------------------------------------------
      integer lun, i, j, status, AT_READ, iaxis_offset,
     +   ichr(640)
      character m(32)*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      nant = 0

      do while (.true.)
         do j = i + 1, 32
            if (ncard.lt.0) then
               card(-ncard) = m(j)
               ncard = ncard - 1
            end if
            if (m(j)(1:8).eq.'ENDTABLE') then
               i = j
               goto 999
            else if (m(j)(1:8).eq.'HEADER' ) then
            else if (m(j)(1:8).eq.'COMMENT') then
            else
               nant = nant + 1
               read(m(j),100) ant_num(nant), sta(nant),
     +            ant_mount(nant), x(nant), y(nant), z(nant),
     +            Iaxis_offset
               axis_offset(nant) = iaxis_offset/1000.0
            end if
         end do

         status = AT_READ (lun, ichr)
         write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
         i = 0
      end do

  100 format (i2,1x,a8,1x,i1,3(1x,f13.3),1x,i4)
  999 an_found = .true.

      return
      end

      SUBROUTINE WRITE_AN_TABLE (i, m)
C-----------------------------------------------------------------------
C     routine to write an AN table to an RPFITS file
C
C     RPN 29/9/88
C     mod rpn 11/10/89 remove met info
C-----------------------------------------------------------------------
      integer   i, k, iaxis_offset
      character m(*)*80, header*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      header = 'HEADER      M       X             Y             '//
     +   'Z       AXIS'
      i = i + 1
      write (m(i),'(a)')  'TABLE AN'
      i = i + 1
      write (m(i),'(a)')  header

      do k = 1, nant
         i = i + 1
         iaxis_offset  =  nint(axis_offset(k)*1000.0)
         write (m(i),100) ant_num(k), sta(k), ant_mount(k),
     +      x(k), y(k), z(k), iaxis_offset
      end do

      i = i + 1
      write (m(i),'(a)')  'ENDTABLE'
  100 format (i2,1x,a8,1x,i1,3(1x,f13.3),1x,i4)

      return
      end

      SUBROUTINE READMT (lun, m, i)
C-----------------------------------------------------------------------
C     routine to read a MT table from an RPFITS file
C
C     RPN 11/10/89
C-----------------------------------------------------------------------
      integer   lun, i, j, status, AT_READ, ichr(640)
      character m(32)*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      n_mt = 0
      do while (.true.)
         do j = i+1, 32
            if (ncard.lt.0) then
               card(-ncard) = m(j)
               ncard = ncard - 1
            end if

            if (m(j)(1:8).eq.'ENDTABLE') then
               i = j
               goto 999
            else if (m(j)(1:8).eq.'HEADER' ) then
            else if (m(j)(1:8).eq.'COMMENT') then
            else
               n_mt = n_mt + 1
               read(m(j),100) mt_ant(n_mt), mt_ut(n_mt),
     +            mt_press(n_mt), mt_temp(n_mt), mt_humid(n_mt)
            end if
         end do

         status = AT_READ (lun, ichr)
         write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
         i = 0
      end do

  100 format (i2,1x, f8.1, 1x,f6.1, 2(1x,f5.1))
  999 mt_found = .true.

      return
      end

      SUBROUTINE WRITE_MT_TABLE (i, m)
C-----------------------------------------------------------------------
C     routine to write an MT table to an RPFITS file
C
C     RPN 11/10/89
C-----------------------------------------------------------------------
      integer i, k
      character m(*)*80, header*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      header = 'HEADER UT PRESS  TEMP  HUMID'
      i = i + 1
      write (m(i),'(a)')  'TABLE MT'
      i = i + 1
      write (m(i),'(a)')  header

      do k = 1, n_mt
         i = i + 1
         write (m(i),100) mt_ant(k), mt_ut(k),
     +      mt_press(k), mt_temp(k), mt_humid(k)
      end do

      i = i + 1
      write (m(i),'(a)')  'ENDTABLE'
  100 format (i2,1x, f8.1, 1x,f6.1, 2(1x,f5.1))

      return
      end

      SUBROUTINE READCU (lun, m, i)
C-----------------------------------------------------------------------
C     routine to read a CU table from an RPFITS file
C
C     RPN 22/03/90
C     Modified: HM  15/05/92  Read old (i2) or new (i3) cu_if.
C-----------------------------------------------------------------------
      integer lun, i, j, status, AT_READ, ichr(640)
      character m(32)*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      n_cu = 0
      do while (.true.)
         do j = i + 1,32
            if (ncard.lt.0) then
               card(-ncard) = m(j)
               ncard = ncard - 1
            end if

            if (m(j)(1:8).eq.'ENDTABLE') then
               i = j
               goto 999
            else if (m(j)(1:8).eq.'HEADER' ) then
            else if (m(j)(1:8).eq.'COMMENT') then
            else
               n_cu = n_cu + 1
               read(m(j),100) cu_ut(n_cu), cu_ant(n_cu), cu_if(n_cu),
     +            cu_cal1(n_cu), cu_cal2(n_cu), cu_ch1(n_cu),
     +            cu_ch2(n_cu)
            end if
         end do

         status = AT_READ(lun, ichr)
         write (m, '(32(20a4,:,/))') (ichr(j),j=1,640)
         i = 0
      end do

  100 format (BN, f8.1,1x,i2,1x,i3,f6.1,1x,f6.1,2(1x,i4))
  999 cu_found = .true.

      return
      end

      SUBROUTINE WRITE_CU_TABLE (i, m)
C-----------------------------------------------------------------------
C     routine to write a CU table to an RPFITS file
C
C     RPN 11/10/89
C     Modified HM 15/05/92 Write cu_if as i3.
C-----------------------------------------------------------------------
      integer i, k
      character m(*)*80, header*80
      INCLUDE 'RPFITS.INC'
C-----------------------------------------------------------------------
      header = 'HEADER  ANT IF CALSTART  CALSTOP   CH1  CH2'
      i = i + 1
      write (m(i),'(a)')  'TABLE CU'
      i = i + 1
      write (m(i),'(a)')  header

      do k = 1, n_cu
         i = i + 1
         write  (m(i),100) cu_ut(n_cu), cu_ant(n_cu), cu_if(n_cu),
     +      cu_cal1(n_cu), cu_cal2(n_cu), cu_ch1(n_cu), cu_ch2(n_cu)
      end do

      i = i + 1
      write (m(i),'(a)')  'ENDTABLE'
  100 format (f8.1,1x,i2,1x,i3,f6.1,1x,f6.1,2(1x,i4))

      return
      end
      SUBROUTINE STRIM (STRING, N)
C-----------------------------------------------------------------------
C     STRIM strips off leading blanks and tabs and finds the position
C     of the last non-blank, non-tab character in a string.
C
C     Given and returned:
C          STRING   C**   Input character string.
C
C     Returned:
C          N        I     Position of the last non-blank, non-tab
C                         character.  If there is none, it will be
C                         returned as zero.
C
C     Called:
C          APLNOT: {TXTLEN}
C
C     Algorithm:
C
C     Notes:
C       1) Trailing tabs will each be converted to a single blank.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Apr/28.  Code last modified; 1989/Sep/06.
C
C-----------------------------------------------------------------------
      INTEGER   N, N1, N2
      CHARACTER STRING*(*)
C-----------------------------------------------------------------------
      CALL TXTLEN (STRING, N1, N2)
      IF (N1.LE.N2) THEN
         STRING = STRING(N1:N2)
         N = N2 - N1 + 1
      ELSE
         N = 0
      END IF
C
      RETURN
      END

      SUBROUTINE UDATE (UTDATE)
C-----------------------------------------------------------------------
C To return current UT date as yyyy-mm-dd
C
C Solaris, SunOS 4, Alpha and SGI  Version
C H.May 1998-11-02
C
C     Corrected ny EWG 2009-07-27 to use AIPS routine ZGDATE.  Calling
C     the C functions TIME and GMTIME can be problematic.
C
C   call udate(utdate)
C   utdate  character*12 returned
C
C returns utdate:
C    ' ' on failure
C    or date returned is current UT date yyyy-mm-dd
C-----------------------------------------------------------------------
      INTEGER   IOS, IMON, IDAY, IYEAR4
      INTEGER   TARRAY(3)
      CHARACTER*12 UTDATE
C-----------------------------------------------------------------------
C     STIME = TIME()
C     CALL GMTIME(STIME, TARRAY)
      CALL ZGDATE (TARRAY)
      IYEAR4 = TARRAY(1)
      IMON = TARRAY(2)
      IDAY = TARRAY(3)
      WRITE (UTDATE, '(I4,''-'',I2.2,''-'',I2.2,''  '')',IOSTAT=IOS)
     *   IYEAR4, IMON, IDAY
      IF (IOS.NE.0) UTDATE = ' '
      END

      SUBROUTINE UPCASE (STRING)
C-----------------------------------------------------------------------
C     UPCASE converts all characters in a string to upper case.
C
C     Given and returned:
C          STRING   C**   The character string.
C
C     Called:
C          none
C
C     Algorithm:
C          Subtracts 32 from the ASCII value of the character if it is
C          a lowercase letter, ASCII range 97-122.
C
C     Notes:
C       1)
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Apr/28.  Code last modified; 1989/Sep/06.
C-----------------------------------------------------------------------
      INTEGER   ASCII, N, NCH
      CHARACTER STRING*(*)
C-----------------------------------------------------------------------
C                                       Loop through the string
      NCH = LEN(STRING)
      DO 10 N = 1, NCH
         ASCII = ICHAR(STRING(N:N))
         IF (ASCII.LT.97)  GO TO 10
         IF (ASCII.GT.122) GO TO 10
         ASCII = ASCII - 32
         STRING(N:N) = CHAR(ASCII)
 10      CONTINUE
C
      RETURN
      END

      REAL FUNCTION VAXR4 (r4)
C-----------------------------------------------------------------------
C   VAXR4 converts from VAX REAL to little-endian IEEE REAL.
C-----------------------------------------------------------------------
      real      r4
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (BYTFLP.EQ.3) THEN
         CALL RV2L (R4, VAXR4)
      ELSE
         CALL RV2B (r4, VAXR4)
         END IF
C
 999  RETURN
      END
      INTEGER FUNCTION VAXI4 (i4)
C-----------------------------------------------------------------------
C   VAXI4 converts (copies) VAX INTEGER to little-endian INTEGER.
C-----------------------------------------------------------------------
      INTEGER   I4
C
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (BYTFLP.EQ.3) THEN
         VAXI4 = i4
      ELSE
         CALL IV2B (I4, VAXI4)
         END IF
C
 999  RETURN
      END
LOCAL INCLUDE 'ZATIO'
      INTEGER   BUFSAV(640), TAPBUF(10240), FDVEC(50), TBIND, LUNSAV
      LOGICAL   REREAD
      COMMON /ATIO/ TAPBUF, FDVEC, BUFSAV, TBIND, REREAD, LUNSAV
LOCAL END

      INTEGER FUNCTION AT_OPEN_READ (fname, lun, INTAPE)
C-----------------------------------------------------------------------
C   converted to TAPIO from Fortran direct access: EWG 07/28/09
C-----------------------------------------------------------------------
      INTEGER   LUN, INTAPE
      CHARACTER FNAME*(*)
C
      INTEGER   IERR
      INCLUDE 'ZATIO'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (FNAME(1:5).EQ.'/dev/') THEN
         AT_OPEN_READ = 1
      ELSE
         LUN = 129 - INTAPE
         IF (FNAME.NE.' ') LUN = 87
         CALL FILL (50, 0, FDVEC)
         FDVEC(1) = LUN
         FDVEC(2) = 2560
         FDVEC(3) = (10240 * NBITWD) / 8
         FDVEC(5) = INTAPE
         CALL CHR2H (48, FNAME, 1, FDVEC(7))
         CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         AT_OPEN_READ = IERR
         END IF
      REREAD = .FALSE.
      LUNSAV = 0
C
 999  RETURN
      END

      INTEGER FUNCTION AT_READ (LUN, BUFFER)
C-----------------------------------------------------------------------
      INTEGER   BUFFER(640)
      INTEGER   LUN
C
      INTEGER   J, IERR
      INCLUDE 'ZATIO'
C-----------------------------------------------------------------------
C                                       restore the last.
      IF (REREAD) THEN
C                                       Check consistency
         IF (LUN.NE.LUNSAV) THEN
            AT_READ = 999
C                                       Copy the buffer saved by
C                                       AT_UNREAD to the input buffer
         ELSE
            DO 10 J = 1,640
               BUFFER(J) = BUFSAV(J)
 10            CONTINUE
            REREAD = .FALSE.
            AT_READ = 0
            END IF
C                                       Read the next record
      ELSE
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         AT_READ = IERR
         IF (IERR.EQ.0) THEN
            CALL COPY (640, TAPBUF(TBIND), BUFFER)
         ELSE IF (IERR.EQ.4) THEN
            AT_READ = -1
            END IF
         END IF
C
 999  RETURN
      END

      INTEGER FUNCTION AT_SKIP_EOF (LUN)
C-----------------------------------------------------------------------
C     Returns -1 if successfully skipped to EOF, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   LUN
C
      INCLUDE 'ZATIO'
      INTEGER   IERR
C-----------------------------------------------------------------------
C                                       read until error
 10   CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.EQ.0) THEN
         GO TO 10
      ELSE IF (IERR.EQ.4) THEN
         AT_SKIP_EOF = -1
      ELSE
         AT_SKIP_EOF = IERR
         END IF
      REREAD = .FALSE.
C
 999  RETURN
      END

      INTEGER FUNCTION AT_UNREAD (LUN, BUFFER)
C-----------------------------------------------------------------------
      INTEGER   BUFFER(640)
      INTEGER   LUN
C
      INTEGER   J
      INCLUDE 'ZATIO'
C-----------------------------------------------------------------------
C                                       Save the buffer for "rereading".
      REREAD = .TRUE.
      LUNSAV = LUN
      DO 10 J = 1,640
         BUFSAV(J) = BUFFER(J)
 10      CONTINUE
      AT_UNREAD = 0
C
 999  RETURN
      END

      INTEGER FUNCTION AT_CLOSE (LUN)
C-----------------------------------------------------------------------
      INTEGER   LUN
C
      INTEGER   IERR
      INCLUDE 'ZATIO'
C-----------------------------------------------------------------------
      CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      AT_CLOSE = IERR
C
 999  RETURN
      END
