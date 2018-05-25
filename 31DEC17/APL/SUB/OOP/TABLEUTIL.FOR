C   Table Class utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "TABLE" utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002-2003, 2007-2012, 2015-2016
C;  Associated Universities, Inc. Washington DC, USA.
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
C   Public functions:
C     COPHED (intab, outtab, ierr)
C        Copy header info from one table to another, force create.
C     ANTNO (anttab, subarr, maxant, ierr)
C        Find maximum antenna number in an AN table.
C     ANTNFO (table, subarr, anum, label, type, dim, value,
C           valuec, ierr)
C        Find info about antenna with id=anum
C     SOUNFO (table, suid, label, type, dim, value, valuec, ierr)
C        Find info about a given source id.
C
C   OOA Fronts to AIPS table specific routines:
C   These routines will open the object.
C
C      OANINI (table, opcode, anrow, arrayc, gstia0, degpdy, safreq,
C            rdate, polrxy, ut1utc, datutc, timsys, aname, xyzhan,
C            tframe, numorb, nopcal, numif, anfqid, ierr)
C         Open/create/init AIPS AN table. (ANTINI)
C      OTABAN (table, opcode, anrow, anname, staxyz, orbprm, nosta,
C            mntsta, staxof, diaman, fwhman, poltya, polaa, polca,
C            poltyb, polab, polcb, ierr)
C         Access AIPS AN table (TABAN)
C
C      OBLINI (table, opcode, blrow, numant, numpol, numif, ierr)
C         Open/create/init AIPS BL table (BLINI)
C      OTABBL (table, opcode, blrow, numpol, time, sourid, suba, ant1,
C            ant2, freqid, facmul, facadd, ierr)
C         Access AIPS BL table. (TABBL)
C
C      OBPINI (table, opcode, bprow, numant, numpol, numif, numfrq,
C            bchan, ierr)
C         Open/create/init AIPS BP table (BPINI)
C      OTABBP (table, opcode, bprow, numif, numfrq, numpol, time,
C            interv, sourid, suba, ant, bandw, iffreq, freqid, refant,
C            weight, bndpas, ierr)
C         Access AIPS BP table. (TABBP)
C
C      OBSINI (table, opcode, bsrow, mode, numif, ierr)
C         Open/create/init AIPS BS table (BSINI)
C      OTABBS (table, opcode, bsrow, numif, time, interv, baseln,
C            subarr, stokes, source, vamp, samp, rmbd, mbderr, mbdamb,
C            rsbd, sbderr, sbdamb, rrate, rterr, rtamb, raccel, accerr,
C            rphase, phserr, ierr)
C         Access AIPS BS table. (TABBS)
C
C      OCGINI (table, opcode, cbrow, ierr)
C         Open/create/init AIPS CG table (CGINI)
C      OTABCG (table, opcode, cbrow, freq, bmaj, bmin, bpa, ierr)
C         Access AIPS CG table. (TABCG)
C
C      OCCINI (table, opcode, ccrow, numcol, ierr)
C         Open/create/init AIPS CC table (CCMINI)
C      OTABCC (table, opcode, ccrow, numcol, x, y, flux, type, parms,
C            ierr)
C         Access AIPS CC table. (TABCCM)
C
C      OCLINI (table, opcode, clrow, numant, numpol, numif, nterm,
C            gmmod, ierr)
C         Open/create/init AIPS CL table. (CALINI)
C      OTABCL (table, opcode, clrow, numpol, numif, time, timei, sourid,
C            antno, suba, freqid, ifr, geodly, dopoff, atmos, datmos,
C            mbdely, clock, dclock, disp, ddisp, creal, cimag, delay,
C            rate, weight, refa, ierr)
C         Access AIPS CL table. (TABCAL)
C
C      OCQINI (table, opcode, cqrow, numif, ierr)
C         Open/create/initialize and AIPS CQ table (CQINI).
C      OTABCQ (table, opcode, cqrow, numif, freqid, subarr, fftsiz,
C              nchan, specav, edgefq, chanbw, taper, ovrsmp, zeropd,
C              filter, avtime, nbits, ovrlap, ierr)
C         Access AIPS CQ table (TABCQ).
C
C      OCSINI (table, opcode, csrow, numbem, numpol, numif, ierr)
C         Open/create/init AIPS CS table. (CSINI)
C      OTABCS (table, opcode, csrow, numpol, time, raapp, decapp, bemno,
C            subarr, csfact, csoff, csraof, csdcof, ierr)
C         Access AIPS CS table. (TABCS)
C
C      OFCINI (table, opcode, fcnum, lastr, ierr)
C         Open/create/init AIPS EDIT class FC table (FCINI)
C      OTABFC (table, opcode, fcrow, flgtim, flgant, flgsor, flgchn,
C            flgif, flgstk, flgsub, flgfq, flgnum, flgop, flgit, dtype,
C            dtimes, dfluxs, flgrea, ierr)
C         Access AIPS EDIT class FC table. (TABFC)
C
C      OFGINI (table, opcode, fgrow, ierr)
C         Open/create/init AIPS FG table (FLGINI)
C      OTABFG (table, opcode, fgrow, sourid, suba, freqid, ants, timer,
C         Access AIPS FG table. (TABFLG)
C
C      OFQINI (table, opcode, fqrow, numif, ierr)
C         Open/create/init AIPS FQ table (FQINI)
C      OTABFQ (table, opcode, fqrow, numif, fqid, iffreq, ifchw, iftbw,
C            ifside, bndcod, ierr)
C         Access AIPS FQ table. (TABFQ)
C      OCHNDA (table, opcode, fqrow, nif, foff, isband, finc, bndcod,
C            freqid, ierr)
C         Access AIPS FQ or CH table. (CHNDAT)
C      OCHNCO (tablei, tableo, bif, eif, navg, freqid, ierr)
C         Copies selected portion of FQ or CH table. (CHNCOP)
C
C      OGCINI (table, opcode, gcrow, numpol, numif, numtab, ierr)
C         Open/create/initialize an AIPS GC table (GCINI).
C      OTABGC (table, opcode, gcrow, numpol, numtab, antnum, subarr,
C              freqid, gctype, nterms, xtype, ytype, xvalue, yvalue,
C              rgain, sens, ierr)
C         Access AIPS GC table (TABGC).
C
C      OGPINI (table, opcode, gprow, rcvr, rlong, rlat, rht, ierr)
C         Open/create/init AIPS GP table (GPINI)
C      OTABGP (table, opcode, nxrow, time, prn, az, elev, tectau,
C            tecphs, ierr)
C         Access AIPS GP table. (TABGP)
C
C      OHFINI (table, opcode, hfrow, ierr)
C         Open/create/init AIPS HF table. (HFINI)
C      OTABHF (table, opcode, hfrow,
C            cc1, ic2, ic3, ic4, ic5, ic6, ic7, ic8, ic9, ic10, ic11,
C            ic12, ic13, ic14, ic15, ic16, ic17, ic18, ic19, ic20, ic21,
C            ic22, ic23, ic24, cc25, cc26, cc27, cc28, cc29, cc30, cc31,
C            cc32, cc33, cc34, cc35, cc36, cc37, cc38, dc39, dc40, dc41,
C            dc42, dc43, dc44, dc45, dc46, dc47, dc48, dc49, dc50, rc51,
C            rc52, rc53, rc54, rc55, rc56, rc57, rc58, rc59, rc60, rc61,
C            rc62, rc63, rc64, rc65, rc66, rc67, rc68, rc69, rc70, rc71,
C            rc72, rc73, rc74, rc75, rc76, rc77, ierr)
C         Does I/O to HF table (TABHF)
C
C      OIMINT (table, opcode, imrow, obscod, rdate, numstk, stk1, numif,
C              numchn, reffrq, chanbw, refpix, numpol, numply, correv,
C              ierr)
C         Open/create.initialize and AIPS IM table (IMINIT).
C      OTABIM (table, opcode, imrow, numpol, time, timint, sourid,
C              antnum, subarr, freqid, ifr, freqvr, pdelay, gdelay,
C              prate, grate, disp, ddisp, ierr)
C         Access AIPS IM table (TABIM).
C      OIMINI (table, opcode, ierr)
C         Open/create/init AIPS interferometer model (IM) table
C         (deprecated)
C      OGTDEL (table, cltime, clsrc, clsta, clarr, clfqid, geodly, disp,
C            ddisp, numrow, ierr)
C         Find geometric delay polynomial in IM table
C
C      OMCINI (table, opcode, mcrow, obscod, rdate, nstoke, stoke1,
C            numif, nchan, rfreq, chanbw, refpix, numpol, fftsiz,
C            ovrsmp, zeropd, taper, deltat, ierr)
C         Open/create/initialize an AIPS MC table (MCINI).
C      OTABMC (table, opcode, mcrow, numpol, numif, time, sourid,
C              antnum, subarr, freqid, atmos, datmos, gdelay, grate,
C              clock, dclock, looff, dlooff, disp, ddisp, ierr)
C         Access AIPS MC table (TABMC).
C      OGETMC (table, deltat, cltime, clsrc, clsta, clarr, clfqid,
C            clock, dclock, atmos, datmos, numrow, ierr)
C         Find clock offsets and atmospheric delay in MC table
C
C      ONXINI (table, opcode, nxrow, ierr)
C         Open/create/init AIPS NX table (NDXINI)
C      OTABNX (table, opcode, nxrow, time, dtime, idsour, subarr,
C            vstart, vend, freqid, ierr)
C         Access AIPS NX table. (TABNDX)
C
C      OOBINI (table, opcode, obrow, ierr)
C         Open/create/init AIPS OB table (OBINI)
C      OTABOB (table, opcode, obrow, nosta, subarr, time, pos, vel,
C              sunang, eclipse, orient, ierr)
C         Access AIPS OB table (TABOB)
C
C      OPCINI (table, opcode, pcrow, numpol, numif, numton, iret)
C         Open/create/initialize AIPS PC table (PCINI)
C      OTABPC (table, opcode, pcrow, numpol, time, timint, sourid,
C              antnum, subarr, freqid, cabcal, state, pcfreq, pcreal,
C              pcimag, pcrate, iret)
C         Access AIPS PC table (TABPC)
C
C      OSNINI (table, opcode, snrow, numant, numpol, numif, numnod,
C            gmmod, ranod, decnod, isappl, ierr)
C         Open/create/init AIPS SN table (SNINI)
C      OTABSN (table, opcode, snrow, numpol, time, timei, sourid, antno,
C            suba, freqid, ifr, nodeno, mbdely, disp, ddisp, creal,
C            cimag, delay, rate, weight, refa, ierr)
C         Access AIPS SN table. (TABSN)
C
C      OSUINI (table, opcode, surow, numif, veltyp, veldef, freqid,
C            isurno, sukols, sunumv, ierr)
C         Open/create/init AIPS SU table (SOUINI)
C      OTABSU (table, opcode, surow, idsou, sounam, qual, calcod, flux,
C            freqo, bandw, raepo, decepo, epoch, raapp, decapp, raobs,
C            decobs, lsrvel, lrestf, pmra, pmdec, ierr)
C         Access AIPS SU table. (TABSOU)
C
C      OTYINI (table, opcode, tyrow, numpol, numif, ierr)
C         Open/create/init AIPS TY table (TYINI)
C      OTABTY (table, opcode, tyrow, numpol, numif, time, timei, sourid,
C            antno, suba, freqid, tsys, tant, ierr)
C         Access AIPS TY table. (TABTY)
C
C      OSUELV (table, ant, subarr, sourid, time, ha, elev, za, ierr)
C         Returns source elevation for a given time and antenna.
C      OSUPAN (table, ant, subarr, sourid, time, pangl, ierr)
C         Returns parallactic angle for a given time, source and
C         antenna.
C-----------------------------------------------------------------------
      SUBROUTINE COPHED (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Initialize the OUTTAB to be the same as INTAB. No data is
C   transfered.
C   Inputs:
C      INTAB   C*?  Name of input table object
C      OUTTAB  C*?  Name of output table object
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   IERR
C
      INTEGER   MAXCOL, MAXKEY
C                                        MAXCOL = max number cols.
      PARAMETER (MAXCOL=128)
C                                        MAXKEY = max number keywords.
      PARAMETER (MAXKEY=50)
      CHARACTER TTITLE*56, CTITLE*(24*MAXCOL), CUNITS*(MAXCOL*8),
     *   KEYS(MAXKEY)*8, CDUMMY*1
      INTEGER   TYPE, DIM(7), COLTYP(MAXCOL), COLDIM(MAXCOL), DUMMY,
     *   NCOL, NKEYS, KLOCS(MAXKEY), KVALS(2*MAXKEY), KTYPE(MAXKEY)
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR  = 0
C                                       Open input
      CALL TABOPN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Label
      CALL TABGET (INTAB, 'LABEL', TYPE, DIM, DUMMY, TTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABPUT (OUTTAB, 'LABEL', OOACAR, DIM, DUMMY, TTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       number of columns
      CALL TABGET (INTAB, 'NCOL', TYPE, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABPUT (OUTTAB, 'NCOL', OOAINT, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Column labels
      CALL TABGET (INTAB, 'COLABEL', TYPE, DIM, DUMMY, CTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABPUT (OUTTAB, 'COLABEL', OOACAR, DIM, DUMMY, CTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Column units
      CALL TABGET (INTAB, 'COLUNIT', TYPE, DIM, DUMMY, CUNITS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABPUT (OUTTAB, 'COLUNIT', OOACAR, DIM, DUMMY, CUNITS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Column types
      CALL TABGET (INTAB, 'COLTYPE', TYPE, DIM, COLTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABPUT (OUTTAB, 'COLTYPE', OOAINT, DIM, COLTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Column dimensions
      CALL TABGET (INTAB, 'COLDIM', TYPE, DIM, COLDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABPUT (OUTTAB, 'COLDIM', OOAINT, DIM, COLDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy all keywords
      KEYS(1) = ' '
      NKEYS = MAXKEY
      CALL TABKGT (INTAB, KEYS, NKEYS, KLOCS, KVALS, KTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close input table
      CALL TABCLO (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open output to create
      CALL TABOPN (OUTTAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Keywords
      CALL TABKPT (OUTTAB, KEYS, NKEYS, KLOCS, KVALS, KTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close output table
      CALL TABCLO (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE ANTNO (TABLE, SUBARR, MAXANT, IERR)
C-----------------------------------------------------------------------
C   Finds maximum antenna number in AN table.
C   Object TABLE need not be an AN table but have one associated.
C   Inputs:
C      TABLE    C*?   Name of table object with associated AN table.
C      SUBARR   I     Desired subarray
C   Output:
C      MAXANT  I    The maximum antenna number.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      INTEGER   SUBARR, MAXANT, IERR
C
      CHARACTER ANTTAB*36, COLLAB*24, CDUMMY*1
      INTEGER   TYPE, DIM(3), NROW, NLAB, ICOL, IROW, ANUMB, DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Name of antenna number column
      DATA COLLAB /'NOSTA'/
C-----------------------------------------------------------------------
      IERR = 0
      MAXANT = 0
C                                       Shallow copy of object
      ANTTAB = 'Temporary table for ANTNO'
      CALL TBCOPY (TABLE, ANTTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (ANTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'AN', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Version
      DIM(1) = 1
      CALL TABPUT (ANTTAB, 'VER', OOAINT, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open AN table
      CALL TABOPN (ANTTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Number of rows?
      CALL TABGET (ANTTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Find antenna no. column
      NLAB = 1
      CALL TABCOL (ANTTAB, NLAB, COLLAB, ICOL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Loop through table
      DO 100 IROW = 1,NROW
         CALL TABDGT (ANTTAB, IROW, ICOL, TYPE, DIM, ANUMB, CDUMMY,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
         MAXANT = MAX (MAXANT, ANUMB)
 100     CONTINUE
C                                       Close AN table
      CALL TABCLO (ANTTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Delete temp object
      CALL TABDES (ANTTAB, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'ANTNO: PROBLEM WITH TABLE' // ANTTAB
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE ANTNFO (TABLE, SUBARR, ANUM, LABEL, TYPE, DIM, VALUE,
     *   VALUEC, IERR)
C-----------------------------------------------------------------------
C   Finds information about an antenna with id=ANUM and column label
C   LABEL (e.g. 'STAXYZ' = antenna coordinates, 'ANNAME' = name)
C   Object TABLE need not be an AN table but have one associated.
C   Inputs:
C      TABLE    C*?   Name of table object with associated AN table.
C      SUBARR   I     Desired subarray
C      ANUM     I     Antenna number
C      LABEL    C*?   Information (column) label.
C   Output:
C      TYPE     I     Data type of VALUE: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value array
C      VALUEC   C*(*) Character value array.
C      IERR     I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), LABEL*(*), VALUEC*(*)
      INTEGER   SUBARR, ANUM, TYPE, DIM(*), VALUE(*), IERR
C
      CHARACTER ANTTAB*36, COLLAB(2)*24, CDUMMY*1
      INTEGER   NROW, NLAB, ICOL(2), IROW, ANUMB, DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Names of column label
      DATA COLLAB /'NOSTA', '  '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Good antenna ID?
      IF (ANUM.LE.0) THEN
         MSGTXT = 'INVALID ANTENNA ID'
         CALL MSGWRT (7)
         IERR = 9
         ANTTAB = TABLE
         GO TO 900
         END IF
C                                       Shallow copy of object
      ANTTAB = 'Temporary table for ANTNFO'
      CALL TBCOPY (TABLE, ANTTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (ANTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'AN', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Version
      DIM(1) = 1
      CALL TABPUT (ANTTAB, 'VER', OOAINT, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open AN table
      CALL TABOPN (ANTTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Number of rows?
      CALL TABGET (ANTTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Find column numbers
      NLAB = 2
      COLLAB(2) = LABEL
      CALL TABCOL (ANTTAB, NLAB, COLLAB, ICOL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Loop through looking for antenna
      DO 100 IROW = 1,NROW
         CALL TABDGT (ANTTAB, IROW, ICOL(1), TYPE, DIM, ANUMB, CDUMMY,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
C                                       Found it.
         IF (ANUM.EQ.ANUMB) THEN
            CALL TABDGT (ANTTAB, IROW, ICOL(2), TYPE, DIM, VALUE,
     *         VALUEC, IERR)
            IF (IERR.GT.0) GO TO 900
            GO TO 500
            END IF
 100     CONTINUE
C                                       Close AN table
 500  CALL TABCLO (ANTTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Delete temp object
      CALL TABDES (ANTTAB, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'ANTNFO: PROBLEM WITH TABLE ' // ANTTAB
      CALL MSGWRT (6)
      MSGTXT = ' FINDING VALUES FOR COL: ' // LABEL
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE SOUNFO (TABLE, SUID, LABEL, TYPE, DIM, VALUE, VALUEC,
     *   IERR)
C-----------------------------------------------------------------------
C   Finds information about a source with  id=SUID and column label
C   LABEL (e.g. 'RAEPO' = RA at standard epoch, 'SOURCE' = name)
C   Inputs:
C   Object TABLE need not be an SU table but have one associated.
C      TABLE    C*?   Name of table object with associated SU table.
C      SUID     I     Antenna number
C      LABEL    C*?   Information (column) label.
C   Output:
C      TYPE     I     Data type of VALUE: 1=double, 2=real,
C                        3=character, 4=integer, 5=logical, 7=bit
C                        arrays.
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value array
C      VALUEC   C*(*) Character value array.
C      IERR     I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), LABEL*(*), VALUEC*(*)
      INTEGER   SUID, TYPE, DIM(*), VALUE(*), IERR
C
      CHARACTER SOUTAB*36, COLLAB(2)*24, CDUMMY*1
      INTEGER   NROW, NLAB, ICOL(2), IROW, SOURID, DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Names of column label
      DATA COLLAB /'ID. NO.', '  '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Good source ID?
      IF (SUID.LE.0) THEN
         MSGTXT = 'INVALID SOURCE ID'
         CALL MSGWRT (7)
         IERR = 9
         SOUTAB = TABLE
         GO TO 900
         END IF
C                                       Shallow copy of object
      SOUTAB = 'Temporary table for SOUNFO'
      CALL TBCOPY (TABLE, SOUTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (SOUTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SU', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Version
      DIM(1) = 1
      CALL TABPUT (SOUTAB, 'VER', OOAINT, DIM, 1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open SU table
      CALL TABOPN (SOUTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Number of rows?
      CALL TABGET (SOUTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Find column numbers
      NLAB = 2
      COLLAB(2) = LABEL
      CALL TABCOL (SOUTAB, NLAB, COLLAB, ICOL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Loop through looking for source
      DO 100 IROW = 1,NROW
         CALL TABDGT (SOUTAB, IROW, ICOL(1), TYPE, DIM, SOURID, CDUMMY,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
C                                       Found it.
         IF (SUID.EQ.SOURID) THEN
            CALL TABDGT (SOUTAB, IROW, ICOL(2), TYPE, DIM, VALUE,
     *         VALUEC, IERR)
            IF (IERR.GT.0) GO TO 900
            GO TO 500
            END IF
 100     CONTINUE
C                                       Close SU table
 500  CALL TABCLO (SOUTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Delete temp object
      CALL TABDES (SOUTAB, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'SOUNFO: PROBLEM WITH TABLE ' // SOUTAB
      CALL MSGWRT (6)
      MSGTXT = ' FINDING VALUES FOR COL: ' // LABEL
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
      END
LOCAL INCLUDE 'TABSTUFF.INC'
C                                        Local Info for table access
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INTEGER   TBKOLS(128,MAXIO), TBNUMV(128,MAXIO)
      COMMON /TBSTUF/ TBKOLS, TBNUMV
LOCAL END
      SUBROUTINE OANINI (TABLE, OPCODE, ANROW, ARRAYC, GSTIA0, DEGPDY,
     *   SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *   TFRAME, NUMORB, NOPCAL, NUMIF, ANFQID, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes antenna tables.
C   Inputs:
C      TABLE        C*?  Table object name
C      OPCODE       C*4  Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C   Input/output (file keywords):
C      ARRAYC(3)    D    Array center X coord. (meters, earth center)
C      GSTIA0       D    GST at time=0 (degrees) on ref. date
C      DEGPDY       D    Earth rotation rate (deg/IAT day)
C      SAFREQ       D    Obs. Reference Frequency for subarray(Hz)
C      RDATE        C*8  Reference date as 'DD/MM/YY'
C      POLRXY(2)    R    Polar position X,Y (meters) on ref. date
C      UT1UTC       R    UT1-UTC  (time sec.)          "
C      DATUTC       R    data time-UTC  (time sec.)          "
C      TIMSYS       C*8  Time system, 'IAT' or 'UTC'
C      ANAME        C*8  Array name
C      XYZHAN       C*8  Handedness of antenna locations
C      TFRAME       C*8  Reference frame for antenna locations
C      NUMORB       I    Number of orbital parameters
C      NOPCAL       I    Number of polarization calibration constants.
C      NUMIF        I    Number of IFs
C      ANFQID       I    Table keyword, denotes the FQ ID for which
C                        the AN parms have been modified. On O/P if
C                        ANFQID = -999 it is not in the table, if
C                        ANFQID = -1 the virgin values still exist, or
C                        the data have no FREQID random parameter.
C   Output:
C      ANROW        I    Next scan number, start of the file if READ,
C                        the last+1 if WRITE
C      IERR         I    Return error code, 0=>OK, else TABINI or TABIO
C                        error.
C-----------------------------------------------------------------------
      INTEGER   ANROW, NUMORB, NOPCAL, NUMIF, ANFQID, IERR
      CHARACTER TABLE*(*), OPCODE*4, ANAME*8, TIMSYS*8, RDATE*8,
     *   XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'AN') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT AN'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL ANTINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, ANROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), ARRAYC, GSTIA0,
     *   DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *   XYZHAN, TFRAME, NUMORB, NOPCAL, NUMIF, ANFQID, IERR)
      IF (IERR.GT.0) GO TO 900
      IF (NOPCAL.GT.0) NUMIF = TBNUMV(9,BUFNO) / NOPCAL
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, ANROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABAN (TABLE, OPCODE, ANROW, ANNAME, STAXYZ, ORBPRM,
     *   NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA, POLCA,
     *   POLTYB, POLAB, POLCB, IERR)
C-----------------------------------------------------------------------
C   Does I/O to Antenna tables. Usually used after setup by OANINI.
C   Inputs:
C      TABLE    C*?     Table object name
C      OPCODE   C*4     Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C   Input/output: (written to or read from antenna file)
C      ANROW    I       Next scan number to read or write.
C      ANNAME   C*8     Station name
C      STAXYZ   D(3)    X,Y,Z offset from array center
C      ORBPRM   D(?)    Orbital parameters.
C      NOSTA    I       Station number
C      MNTSTA   I       Mount type, 0=altaz, 1=equatorial, 2=orbiting
C      STAXOF   R       Axis offset
C      DIAMAN   R       Antenna diameter (m)
C      FWHMAN   R(*)    FWHM single-dish beam in deg scaled to 1 GHz
C      POLTYA   C*2     Feed A feed poln. type 'R','L','X','Y'
C      POLAA    R       Feed A feed position angle.
C      POLCA    R(?)    Feed A poln. cal parameter. (note 2)
C      POLTYB   C*2       Feed B feed poln. type 'R','L','X','Y'
C      POLAB    R       Feed B feed position angle.
C      POLCB    R(?)    Feed B poln. cal parameters.
C   Output:
C      IERR     I       Error code, 0=>OK else TABIO error.
C                       Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      INTEGER   ANROW, NOSTA, MNTSTA, IERR
      CHARACTER TABLE*(*), OPCODE*4, ANNAME*8, POLTYA*2, POLTYB*2
      REAL      STAXOF, DIAMAN, FWHMAN(*), POLAA, POLCA(*), POLAB,
     *   POLCB(*)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABAN (OPCODE, OBUFFR(OFF,BUFNO), ANROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *   DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OBLINI (TABLE, OPCODE, BLROW, NUMANT, NUMPOL, NUMIF,
     *   IERR)
C-----------------------------------------------------------------------
C   Creates and initializes baseline correction (BL) extension tables.
C    Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C    Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of polarizations.
C     NUMIF        I    Number of IFs
C    Output:
C     BLROW        I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   BLROW, NUMANT, NUMPOL, NUMIF, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'BL') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT BL'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL BLINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, BLROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   NUMANT, NUMPOL, NUMIF, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, BLROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABBL (TABLE, OPCODE, BLROW, NUMPOL, TIME, SOURID,
     *   SUBA, ANT1, ANT2, FREQID, FACMUL, FACADD, IERR)
C-----------------------------------------------------------------------
C   Does I/O to baseline (BL) extension tables. Usually used after
C   setup by OBLINI.
C    Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     BLROW       I   Next entry number to read or write.
C     NUMPOL       I   Number of polarizations per IF.
C    Input/output: (written to or read from baseline file)
C     TIME         R   Center time of record (Days)
C     SOURID       I   Source ID number.
C     SUBA         I   Subarray number.
C     ANT1         I   First antenna number.
C     ANT2         I   Second antenna number.
C     FREQID       I   Freqid #
C     FACMUL(2,2,m)R   Multiplicative correction,  m IFs
C                      second dimension is polarization,
C                      (1,*,*) = real, (2,*,*) = imag.
C     FACADD(2,2,m)R   Additive correction, m IFs
C    Output:
C     BLROW       I   Next solution number.
C     IERR         I   Error code, 0=>OK else TABIO error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   BLROW, NUMPOL, SOURID, ANT1, ANT2, SUBA, FREQID, IERR
      REAL      TIME, FACMUL(2,2,*), FACADD(2,2,*)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABBL (OPCODE, OBUFFR(OFF,BUFNO), BLROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),
     *   NUMPOL, TIME, SOURID, SUBA, ANT1, ANT2, FREQID, FACMUL, FACADD,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OBPINI (TABLE, OPCODE, BPROW, NUMANT, NUMPOL, NUMIF,
     *   NUMFRQ, BCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IERR)
C-----------------------------------------------------------------------
C    Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C    Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of polarizations.
C     NUMIF        I    Number of IFs
C     NUMFRQ       I    Number of frequency channels
C     BCHAN        I    Start channel number
C     NUMSHF       I    Maximum number of frequency shifted entries.
C     LOWSHF       R    Most negative shift
C     DELSHF       R    Shift increment
C     LBPTYP       C*7  BP type (non-blank for polynomial bandpasses).
C    Output:
C     BPROW        I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else BPINI
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, LBPTYP*8
      INTEGER    BPROW, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN,
     *   NUMSHF, IERR
      REAL       LOWSHF, DELSHF
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'BP') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT BP'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL BPINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, BPROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), NUMANT, NUMPOL,
     *   NUMIF, NUMFRQ, BCHAN, NUMSHF, LOWSHF, DELSHF, LBPTYP, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, BPROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABBP (TABLE, OPCODE, BPROW, NUMIF, NUMFRQ, NUMPOL,
     *   TIME, INTERV, SOURID, SUBA, ANT, BANDW, IFFREQ, FREQID, REFANT,
     *   WEIGHT, BNDPAS, IERR)
C-----------------------------------------------------------------------
C   Does I/O to bandpass (BP) extension tables. Usually used after
C   setup by OBPINI.
C   Inputs:
C      TABLE   C*?      Table object name
C      OPCODE  C*4      Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C      BPROW  I        Next entry number to read or write.
C      NUMIF   I        Number of IF's
C      NUMFRQ  I        Number of chns
C      NUMPOL  I        Number of polarizations per IF.
C   Input/output: (written to or read from BP file)
C      TIME    D        Center time of record (Days)
C      INTERV  R        Time interval of record (Days)
C      SOURID  I        Source ID number.
C      SUBA    I        Subarray number.
C      ANT     I        Antenna number.
C      BANDW   R        Bandwidth of an individual channel (Hz)
C      IFFREQ  D(m)     Reference frequency for each IF (Hz)
C      FREQID  I        Freq. id number
C      REFANT  I(2)     Reference Antenna; one for each poln
C      WEIGHT  R(if,p)  Weights: (IF,poln)
C      BNDPAS  C(*)     Complex bandpass (channels, IFs, polns)
C   Output:
C      BPROW    I      Next solution number.
C      IERR     I      Error code, 0=>OK else TABBP error.
C                      Note: -1=> read but all polzn #1 flagged
C                            -2=> read but all polzn #2 flagged
C                            -3=> both flagged
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   BPROW, NUMIF, NUMFRQ, NUMPOL, SOURID, SUBA, ANT,
     *   FREQID, REFANT(2), IERR
      DOUBLE PRECISION TIME, IFFREQ(MAXIF)
      REAL      INTERV, WEIGHT(*), BNDPAS(*), BANDW
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABBP (OPCODE, OBUFFR(OFF,BUFNO), BPROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),NUMIF, NUMFRQ, NUMPOL, TIME, INTERV, SOURID,
     *   SUBA, ANT, BANDW, IFFREQ, FREQID, REFANT, WEIGHT, BNDPAS, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OBSINI (TABLE, OPCODE, BSROW, MODE, NUMIF, IERR)
C-----------------------------------------------------------------------
C   Inputs:
C    TABLE    C*(*)         Table object name
C    OPCODE   C*4           Operation code:
C                            'WRIT' - open for reading and writing,
C                                     create if necessary
C                            'READ' - open for reading only
C   Input/output:
C    MODE     C*4           Solution mode used to derive the
C                            values in the table
C                            'INDE' - independent delays for each IF
C                            'VLBA' - one delay for all IFs
C                            'MK3 ' - multiband and single-band
C                                     delays
C                            'RATE' - rate only
C    NUMIF    I             Number of IFs in table (unity unless
C                            MODE = 'INDE'
C   Output:
C    BSROW    I             Next record to read or write: 1 if OPCODE
C                            is 'READ', one greater than the current
C                            number of records if OPCODE is 'WRIT'
C    IERR     I             Return error code: 0 - OK
C                                              1 - error
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, MODE*4
      INTEGER   BSROW, NUMIF, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'BS') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT BP'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C
      CALL BSINI (OPCODE, OBUFFR(OFF, BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, BSROW, TBKOLS(1, BUFNO), TBNUMV(1, BUFNO), MODE, NUMIF,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, BSROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABBS (TABLE, OPCODE, BSROW, NUMIF, TIME, INTERV,
     *   BASELN, SUBARR, STOKES, SOURCE, VAMP, SAMP, RMBD,
     *   MBDERR, MBDAMB, RSBD, SBDERR, SBDAMB, RRATE, RTERR, RTAMB,
     *   RACCEL, ACCERR, RPHASE, PHSERR, IERR)
C-----------------------------------------------------------------------
C   Read, write or close a baseline fringe solution (BS) table.  Usually
C   used after setup be OBSINI.
C
C   Inputs:
C    TABLE     C*(*)           Table object name
C    OPCODE    C*4             Operation code:
C                               'READ' - read a row from the table
C                               'WRIT' - write a row to the table
C                               'CLOS' - close table file and flush
C                                        changes.
C    NUMIF     I               Number of IF's in table (should be 1 if
C                               multiband delays are present)
C   Input/output:
C    BSROW     I               Next row to read or write
C    TIME      D               Center time of record in days
C    INTERV    R               Time interval covered by record in days
C    BASELN    I(2)            Antennae in baseline
C    SUBARR    I               Subarray number
C    STOKES    I               UV stokes code
C    SOURCE    I               Source ID number
C    VAMP      R(*)            Vector amplitude/Jy
C    SAMP      R(*)            Scalar amplitude/Jy
C    RMBD      R               Residual multiband delay/sec
C    MBDERR    R               Multiband delay error/sec
C    MBDAMB    R               Multiband delay ambiguity/sec
C    RSBD      R(*)            Residual single-band delay/sec
C    SBDERR    R(*)            Single-band delay error/sec
C    SBDAMB    R               Single-band delay ambiguity/sec
C    RRATE     R(*)            Residual fringe rate/Hz
C    RTERR     R(*)            Fringe rate error/Hz
C    RTAMB     R               Fringe rate ambiguity/Hz
C    RACCL    R(*)             Residual fringe acceleration/Hz**2
C    ACCERR   R(*)             Fringe acceleration error/Hz**2
C    RPHASE   R(*)             Residual phase/degrees
C    PHSERR   R(*)             Phase error/degrees
C
C   Outputs:
C    IERR      I               Error code: -1 - row flagged
C                                           0 - OK
C                                           1 - read/write beyond end
C                                           2 - error
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   BSROW, NUMIF, BASELN(2), SUBARR, STOKES, SOURCE, IERR
      REAL      INTERV, VAMP(*), SAMP(*), RMBD, MBDERR, MBDAMB, RSBD(*),
     *   SBDERR(*), SBDAMB, RRATE(*), RTERR(*), RTAMB, RACCEL(*),
     *   ACCERR(*), RPHASE(*), PHSERR(*)
      DOUBLE PRECISION TIME
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABBS (OPCODE, OBUFFR(OFF, BUFNO), BSROW, TBKOLS(1, BUFNO),
     *      TBNUMV(1, BUFNO), NUMIF, TIME, INTERV, BASELN, SUBARR,
     *      STOKES, SOURCE, VAMP, SAMP, RMBD, MBDERR, MBDAMB, RSBD,
     *      SBDERR, SBDAMB, RRATE, RTERR, RTAMB, RACCEL, ACCERR, RPHASE,
     *      PHSERR, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCGINI (TABLE, OPCODE, CGROW, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes Clean beam (CG) extension tables.
C   Inputs:
C      TABLE        C*? Table object name
C      OPCODE       C*4 Operation code:
C                       'WRIT' = create/init for write or read
C                       'READ' = open for read only
C   Output:
C      CGROW        I   Next record number, start of the file if 'READ',
C                       the last+1 if WRITE
C      IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                       error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   CGROW, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3), TABVER
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'CG') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT CG'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL CGINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, CGROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), TABVER, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, CGROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABCG (TABLE, OPCODE, CGROW, FREQ, BMAJ, BMIN, BPA,
     *   IERR)
C-----------------------------------------------------------------------
C   Does I/O to Clean beam (CG) extension tables. Usually used after
C   setup by OCGINI.
C   Inputs:
C      TABLE        C*? Table object name
C      OPCODE       C*4 Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C   Input/output: (written to or read from baseline file)
C      CGROW        I   Next entry number to read or write.
C      FREQ         D   Frequency Hz
C      BMAJ         R   Major axis (degrees)
C      BMIN         R   Minor axis (degrees)
C      BPA          R   position angle (degrees)
C   Output:
C      IERR         I   Error code, 0=>OK else TABCG error.
C                       Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   CGROW, IERR
      REAL      BMAJ, BMIN, BPA
      DOUBLE PRECISION FREQ
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 900
         END IF
C                                        Transfer
      CALL TABCG (OPCODE, OBUFFR(OFF,BUFNO), CGROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), FREQ, BMAJ, BMIN, BPA, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCCINI (TABLE, OPCODE, CCROW, NUMCOL, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes Component model (CC) extension tables.
C   Inputs:
C      TABLE        C*? Table object name
C      OPCODE       C*4 Operation code:
C                       'WRIT' = create/init for write or read
C                       'READ' = open for read only
C   Input/output
C      NUMCOL       I   Number of columns, 3 => point components only
C   Output:
C      CCROW        I   Next scan number, start of the file if 'READ',
C                       the last+1 if WRITE
C      IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                       error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   CCROW, NUMCOL, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'CC') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT CC'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL CCMINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, CCROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),  NUMCOL, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, CCROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABCC (TABLE, OPCODE, CCROW, NUMCOL, X, Y, FLUX, TYPE,
     *   PARMS, IERR)
C-----------------------------------------------------------------------
C   Does I/O to Component model (CC) extension tables. Usually used
C   after setup by OCCINI.
C   Inputs:
C      TABLE        C*? Table object name
C      OPCODE       C*4 Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C   Input/output: (written to or read from baseline file)
C      CCROW        I   Next entry number to read or write.
C      NUMCOL       I   Number of columns, 3 => point components only
C      X            R   "X" position in degrees
C      Y            R   "Y" position in degrees
C      FLUX         R   Model flux density (Jy)
C      TYPE         I   Model type: (NUMCOL > 3)
C                       0 = point
C                       1 = Gaussian on sky
C                       2 = Convolved Gaussian
C                       3 = Uniform optically thin sphere
C      PARMS        R(*) Model parameters (NUMCOL > 3)
C                        (min of 3 for Gaussians)
C   Output:
C      IERR         I   Error code, 0=>OK else TABCCM error.
C                       Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   CCROW, TYPE, NUMCOL, IERR
      REAL      X, Y, FLUX, PARMS(*)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 900
         END IF
C                                        Transfer
      CALL TABCCM (OPCODE, OBUFFR(OFF,BUFNO), CCROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), NUMCOL, X, Y, FLUX, TYPE, PARMS, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCLINI (TABLE, OPCODE, CLROW, NUMANT, NUMPOL, NUMIF,
     *   NTERM, GMMOD, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes calibration extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of IFs per pair
C     NUMIF        I    Number of IF pairs
C     NTERM        I    Number of terms in model polynomial.
C     GMMOD        R    Mean gain modulus
C   Output:
C     CLROW       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   CLROW, NUMANT, NUMPOL, NUMIF, NTERM, IERR
      REAL      GMMOD
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'CL') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT CL'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL CALINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, CLROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   NUMANT, NUMPOL, NUMIF, NTERM, GMMOD, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, CLROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABCL (TABLE, OPCODE, CLROW, NUMPOL, NUMIF, TIME,
     *   TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, GEODLY, DOPOFF, ATMOS,
     *   DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG,
     *   DELAY, RATE, WEIGHT, REFA, IERR)
C-----------------------------------------------------------------------
C   Does I/O to CALIBRATION extension tables. Usually used after setup
C   by OCLINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     CLROW       I   Next scan number to read or write.
C     NUMPOL       I   Number of polarizations per IF.
C     NUMIF        I   Number of IFs.
C   Input/output: (written to or read from CAL file)
C     TIME         D   Center time of CAL record (Days)
C     TIMEI        R   Time interval covered by record (days)
C     SOURID       I   Source ID as defined in the SoUrce table.
C     ANTNO        I   Antenna number as defined in Antenna table
C     SUBA         I   Subarray number.
C     FREQID       I   Freqid # as defined in FQ table.
C     IFR          R   Ionospheric Faraday Rotation (rad/m**2)
C     GEODLY(*)    D   Geometric delay polynomial series at TIME (sec)
C     DOPOFF(*)    R   Doppler offset for each IF (Hz)
C     ATMOS(2)     R   Atmospheric delay (sec) 1/Poln
C     DATMOS(2)    R   Time derivative of ATMOS (sec/sec)
C     MBDELY(2)    R   Multi band delays (sec) 1/Poln
C     CLOCK(2,*)   R   "Clock" epoch error (sec) 1/poln
C     DCLOCK(2,*)  R   Time derivative of CLOCK (sec/sec)
C     DISP(2,*)    R   Dispersive delay (sec at wavelength = 1m) 1/poln
C     DDISP(2,*)   R   Time derivative of DISP (sec/sec)
C     CREAL(2,*)   R   Real part of the complex gain, 1/poln/IF
C     CIMAG(2,*)   R   Imag part of the complex gain, 1/poln/IF
C     DELAY(2,*)   R   Residual group delay (sec), 1/poln/IF
C     RATE(2,*)    R   Residual fringe rate (Hz), 1/poln/IF
C     WEIGHT(2,*)  R   Weight of solution, 1/poln/IF
C     REFA(2,*)    I   Reference antenna use for cal. solution.
C   Output:
C     CLROW        I   Next CAL number.
C     IERR         I   Error code, 0=>OK else TABCAL error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      DOUBLE PRECISION TIME, GEODLY(*)
      REAL      TIMEI, IFR, DOPOFF(*), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,*),
     *   CIMAG(2,*), DELAY(2,*), RATE(2,*), WEIGHT(2,*)
      INTEGER   CLROW, NUMPOL, NUMIF, SOURID, ANTNO, SUBA, FREQID,
     *   REFA(2,*), IERR
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABCAL (OPCODE, OBUFFR(OFF,BUFNO), CLROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),
     *   NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *   GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *   DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCSINI (TABLE, OPCODE, CSROW, NUMBEM, NUMPOL, NUMIF,
     *   IERR)
C-----------------------------------------------------------------------
C   Creates and initializes single-dish calibration extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Input/output
C     NUMBEM       I    Number of beams
C     NUMPOL       I    Number of IFs per pair
C     NUMIF        I    Number of IF pairs
C   Output:
C     CSROW       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   CSROW, NUMBEM, NUMPOL, NUMIF, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'CS') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT CS'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL CSINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, CSROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   NUMBEM, NUMPOL, NUMIF, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, CSROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCQINI (TABLE, OPCODE, CQROW, NUMIF, IRET)
C-----------------------------------------------------------------------
C   Open a CQ table.
C
C   If TABLE references an existing CQ table and OPCODE is 'READ' then
C   open the table for reading, set CQROW to 1, set NUMIF to the value
C   of the NO_IF keyword in the table header, and set IRET to zero.
C
C   If TABLE references an existing CQ table and OPCODE is 'WRIT' then
C   open the table for reading and writing, set CQROW to one more than
C   the number of rows in the table, set NUMIF to the value of the
C   NO_IF keyword in the table header, and set IRET to zero.
C
C   If TABLE references a CQ table that does not exist and OPCODE is
C   'WRIT' then create the CQ table, open it for reading and writing,
C   set the value of the NO_IF keyword in the table header to NUMIF,
C   set CQROW to 1, and set IRET to zero.
C
C   If TABLE does not reference a CQ table, OPCODE is not 'READ' or
C   'WRIT', or if it is not possible to open the table then issue
C   one or more error messages and set IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)    name of table object
C      OPCODE  C*4      open mode: 'READ' or 'WRIT'
C
C   Input/Output:
C      NUMIF   I        number of IFs covered by table
C
C   Output:
C      CQROW   I        next row to read or write
C      IRET    I        return status: 0 if table opened,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      CHARACTER OPCODE*4
      INTEGER   CQROW
      INTEGER   NUMIF
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'TABSTUFF.INC'
C
C     Local variables:
C
C     BUFNUM   allocated buffer number
C     OFFSET   offset of I/O region in allocated buffer
C
C     TDISK    disk number for table
C     TCNO     catalogue number for table
C     TTYPE    table type
C     TVER     table version number
C
C     LUN      LUN allocated for table I/O
C
C     DIM      object attribute dimensions
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   TDISK
      INTEGER   TCNO
      CHARACTER TTYPE*2
      INTEGER   TVER
C
      INTEGER   LUN
C
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve table information and set IRET to 0 or set IRET to a non-
C     zero value if the table information can not be retrieved:
C
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (TTYPE .EQ. 'CQ') THEN
C
C           Assign an I/O buffer for the table and set IRET to 0 or set
C           IRET to a non-zero value if no buffers are available:
C
            CALL OBOPEN (TABLE, IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Retrieve the buffer number and set IRET to 0:
C
               CALL OBINFO (TABLE, BUFNUM, IRET)
C
C              Assign a LUN for the table and set IRET to 0 or set IRET
C              to a non-zero value if no LUNs are available:
C
               CALL OBLUN (LUN, IRET)
C
               IF (IRET .EQ. 0) THEN
C
C                 Retrieve the catalogue header blcok for the parent
C                 file and set IRET to 0 or set IRET to a non-zero
C                 value if the catalogue header block can not be
C                 retrieved:
C
                  CALL OBHGET (TABLE, CATBLK, IRET)
C
                  IF (IRET .EQ. 0) THEN
C
C                    Open the table, update TVER to the actual version
C                    number of the table if it was zero before, and set
C                    IRET to 0 or set IRET to a non-zero value if it is
C                    not possible to open the table:
C
                     CALL CQINI (OPCODE, OBUFFR(OFFSET, BUFNUM), TDISK,
     *                           TCNO, TVER, CATBLK, LUN, CQROW,
     *                           TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM),
     *                           NUMIF, IRET)
C
                     IF (IRET .EQ. 0) THEN
C
C                       Record the actual version number:
C
                        DIM(1) = 1
                        DIM(2) = 1
                        DIM(3) = 0
                        CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER,
     *                               CDUMMY, IRET)
C
C                       If a new table was created the catalogue header
C                       block is changed and needs to be written back to
C                       disk.
C
                        IF (OPCODE .EQ. 'WRIT') THEN
C
C                          Update the header block and set IRET to 0
C                          or set IRET to a non-zero value if it is not
C                          possible to update the catalogue header:
C
                           CALL OBHPUT (TABLE, CATBLK, IRET)
C
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 7000) IRET
                              CALL MSGWRT (7)
                           END IF
                        END IF
C
C                       The table needs to be closed and re-opened so
C                       that the generic table access routines in APLOOP
C                       will work.
C
                        CALL TABIO ('CLOS', 0, CQROW,
     *                              OBUFFR(OFFSET, BUFNUM),
     *                              OBUFFR(OFFSET, BUFNUM), IRET1)
                        IF (IRET1 .NE. 0) THEN
                           WRITE (MSGTXT, 7001) IRET1
                           CALL MSGWRT (7)
                           IRET = IRET1
                        END IF
                     ELSE
C
C                       Table could not be opened.
C
                        WRITE (MSGTXT, 7002) IRET
                        CALL MSGWRT (7)
                     END IF
                  ELSE
C
C                    Catalogue header could not be retrieved.
C
                     WRITE (MSGTXT, 7003) IRET
                     CALL MSGWRT (7)
                  END IF
C
C                 Free LUN:
C
                  CALL OBLUFR (LUN)
C
               ELSE
C
C                 Unable to allocate LUN.
C
                  WRITE (MSGTXT, 7004) IRET
                  CALL MSGWRT (7)
               END IF
C
C              Free the table buffer and set IRET1 to 0 or set IRET1 to
C              a non-zero value if it is not possible to free the
C              buffer:
C
               CALL OBCLOS (TABLE, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 7005) IRET1
                  CALL MSGWRT (7)
                  IRET = IRET1
               END IF
            ELSE
C
C              Failed to allocate table buffer.
C
               WRITE (MSGTXT, 7006) IRET
               CALL MSGWRT (7)
            END IF
C
C           Re-open the table using the generic APLOOP table interface
C           and set IRET1 to 0 or set IRET1 to a non-zero value if the
C           table can not be re-opened:
C
            CALL TABOPN (TABLE, OPCODE, IRET1)
C
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 7007) IRET1
               CALL MSGWRT (7)
               IRET = IRET1
            END IF
         ELSE
C
C           TABLE does not refer to a CQ table.
C
            WRITE (MSGTXT, 7008) TTYPE
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        Failed to retrieve table information.
C
         WRITE (MSGTXT, 7009) IRET
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OCQINI: FAILED TO UPDATE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7001 FORMAT ('OCQINI: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 7002 FORMAT ('OCQINI: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 7003 FORMAT ('OCQINI: FAILED TO RETREIVE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7004 FORMAT ('OCQINI: FAILED TO ALLOCATE LUN (ERROR ', I4, ')')
 7005 FORMAT ('OCQINI: FAILED TO FREE TABLE BUFFER (ERROR ', I4, ')')
 7006 FORMAT ('OCQINI: FAILED TO ALLOCATE TABLE BUFFER (ERROR ', I4,
     *        ')')
 7007 FORMAT ('OCQINI: FAILED TO RE-OPEN TABLE (ERROR ', I4, ')')
 7008 FORMAT ('OCQINI: TABLE MUST HAVE TYPE ''CQ'' NOT ''', A2, '''')
 7009 FORMAT ('OCQINI: FAILED TO RETRIEVE TABLE INFORMATION (ERROR ',
     *        I4, ')')
      END
      SUBROUTINE OTABCQ (TABLE, OPCODE, CQROW, NUMIF, FREQID, SUBARR,
     *                   FFTSIZ, NCHAN, SPECAV, EDGEFQ, CHANBW, TAPER,
     *                   OVRSMP, ZEROPD, FILTER, AVTIME, NBITS, OVRLAP,
     *                   IRET)
C-----------------------------------------------------------------------
C   Read, write, or close CQ table.
C
C   If TABLE is an open CQ table and OPCODE is 'READ' then set FREQID
C   through OVRLAP from row CQROW of the table, increment CQROW by 1,
C   and set IRET to 0 if row CQROW is not flagged or a negative value if
C   row CQROW is flagged. If TABLE is an open CQ table and OPCODE is
C   'READ' and row number CQROW can not be read then issue one or more
C   error messages and set IRET to a positive value.
C
C   If TABLE is an open CQ table and OPCODE is 'WRIT' then write the
C   values of FREQID through OVRLAP to row CQROW of the table, increment
C   CQROW by 1, and set IRET to 0. If TABLE is an open CQ table and
C   OPCODE is 'WRIT' and row number CQROW can not be written then issue
C   one or more error messages and set IRET to a positive value.
C
C   If TABLE is an open CQ table and OPCODE is 'CLOS' then close the
C   table and set IRET to 0. If TABLE is an open CQ table and OPCODE is
C   'CLOS' and the table can not be closed then issue one or more error
C   messages and set IRET to a non-zero value.
C
C   If TABLE is not an open table then issue an error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)       name of table object
C      OPCODE  C*4         'READ', 'WRIT', or 'CLOS'
C      NUMIF   I           number of IFs
C
C   Input/Output:
C      CQROW   I           row number to read or write; incremented on
C                          output
C      FREQID  I           frequency ID
C      SUBARR  I           subarray number
C      FFTSIZ  I(NUMIF)    FFT size for each IF
C      NCHAN   I(NUMIF)    number of channels for each IF
C      SPECAV  I(NUMIF)    spectral averaging factor for each IF
C      EDGEFQ  D(NUMIF)    edge frequency for each IF in Hz
C      CHANBW  D(NUMIF)    channel bandwidth for each IF in Hz
C      TAPER   C(NUMIF)*8  taper function for each IF
C      OVRSMP  I(NUMIF)    oversampling factor for each IF
C      ZEROPD  I(NUMIF)    zero-padding factor for each IF
C      FILTER  I(NUMIF)    filter type for each IF
C      AVTIME  R(NUMIF)    averaging time for each IF in seconds
C      NBITS   I(NUMIF)    number of bits correlated for each IF
C      OVRLAP  I(NUMIF)    FFT overlap factor for each IF
C
C   Output:
C      IRET    I           return status: 0 if operation complete
C                                         non-zero if operation failed
C-----------------------------------------------------------------------
      CHARACTER        TABLE*(*)
      CHARACTER        OPCODE*4
      INTEGER          CQROW
      INTEGER          NUMIF
      INTEGER          FREQID
      INTEGER          SUBARR
      INTEGER          FFTSIZ(NUMIF)
      INTEGER          NCHAN(NUMIF)
      INTEGER          SPECAV(NUMIF)
      DOUBLE PRECISION EDGEFQ(NUMIF)
      DOUBLE PRECISION CHANBW(NUMIF)
      CHARACTER        TAPER(NUMIF)*8
      INTEGER          OVRSMP(NUMIF)
      INTEGER          ZEROPD(NUMIF)
      INTEGER          FILTER(NUMIF)
      REAL             AVTIME(NUMIF)
      INTEGER          NBITS(NUMIF)
      INTEGER          OVRLAP(NUMIF)
      INTEGER          IRET
C
      INCLUDE 'TABSTUFF.INC'
C
C     Local Variables:
C
C     BUFNUM      number of buffer allocated for table I/O; zero if
C                 table is not open
C     OFFSET      offset to I/O region in allocated buffer
C
C     IRET1       alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve buffer number for TABLE and set IRET to 0 or set IRET to
C     a non-zero value if TABLE is not a defined object:
C
      CALL OBINFO (TABLE, BUFNUM, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (BUFNUM .GT. 0) THEN
C
C           Perform the operation and set IRET accordingly:
C
            CALL TABCQ (OPCODE, OBUFFR(OFFSET, BUFNUM), CQROW,
     *                  TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM), NUMIF,
     *                  FREQID, SUBARR, FFTSIZ, NCHAN, SPECAV, EDGEFQ,
     *                  CHANBW, TAPER, OVRSMP, ZEROPD, FILTER, AVTIME,
     *                  NBITS, OVRLAP, IRET)
            IF (IRET .GT. 0) THEN
               WRITE (MSGTXT, 7000) IRET
               CALL MSGWRT (7)
            END IF
C
C           The table buffer needs to be deallocated when the table is
C           closed.
C
            IF (OPCODE .EQ. 'CLOS') THEN
               CALL OBCLOS (TABLE, IRET1)
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 7001) IRET
                  CALL MSGWRT (7)
               END IF
            END IF
         ELSE
C
C           Table is not open.
C
            WRITE (MSGTXT, 7002)
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        TABLE does not name a recognized table object.
C
         WRITE (MSGTXT, 7003)
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OTABCQ: TABLE I/O FAILED (ERROR ', I4, ')')
 7001 FORMAT ('OTABCQ: FAILED TO DE-ALLOCATE BUFFER (ERROR ', I4, ')')
 7002 FORMAT ('OTABCQ: TABLE IS NOT OPEN')
 7003 FORMAT ('OTABCQ: TABLE OBJECT IS NOT DEFINED')
      END
      SUBROUTINE OTABCS (TABLE, OPCODE, CSROW, NUMPOL, TIME, RAAPP,
     *   DECAPP, BEMNO, SUBARR, CSFACT, CSOFF, CSRAOF, CSDCOF, IERR)
C-----------------------------------------------------------------------
C   Does I/O to single dish CALIBRATION extension tables. Usually used
C   after setup by OCSINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     CSROW       I   Next scan number to read or write.
C     NUMPOL       I   Number of polarizations per IF.
C   Input/output: (written to or read from CAL file)
C     TIME         D   Center time of CAL record (Days)
C     RAAPP        R   Apparent RA (degrees)
C     DECAPP       R   Apparent declination (degrees)
C     BEMNO        I   Beam number
C     SUBARR       I   Subarray number
C     CSFACT(2,*)  R   Calibration factor, 1 /Poln/IF
C     CSOFF(2,*)   R   Calibration offset, 1 /Poln/IF
C     CSRAOF(2,*)  R   RA correction, 1 /Poln/IF
C     CSDCOF(2,*)  R   Dec. correction, 1 /Poln/IF
C   Output:
C     CLROW        I   Next CS row number.
C     IERR         I   Error code, 0=>OK else TABCAL error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      DOUBLE PRECISION TIME
      REAL      RAAPP, DECAPP, CSFACT(2, *), CSOFF(2, *), CSRAOF(2, *),
     *   CSDCOF(2, *)
      INTEGER   CSROW, NUMPOL, BEMNO, SUBARR, IERR
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABCS (OPCODE, OBUFFR(OFF,BUFNO), CSROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), NUMPOL, TIME, RAAPP, DECAPP, BEMNO, SUBARR,
     *   CSFACT, CSOFF, CSRAOF, CSDCOF, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OFCINI (TABLE, OPCODE, FCNUM, LASTR, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes FC extension tables.
C   Inputs:
C     TABLE    C*?   Table object name
C     OPCODE   C*4   Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Output:
C     FCNUM    I     Highest FC command number in file
C     LASTR    I     Highest record number in file
C     IERR     I     Return error code, 0=>OK, else FLGINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   FCNUM, LASTR, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'FC') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT FC'
         CALL MSGWRT (7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL FCINI (OPCODE, LUN, TDISK, TCNO, TVER, CATBLK, FCNUM, LASTR,
     *   OBUFFR(OFF,BUFNO), TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, LASTR, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABFC (TABLE, OPCODE, FCROW, FLGTIM, FLGANT, FLGSOR,
     *   FLGCHN, FLGIF, FLGSTK, FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT,
     *   DTYPE, DTIMES, DFLUXS, FLGREA, IERR)
C-----------------------------------------------------------------------
C   Does I/O to flag command (FC) extension tables (EDIT Class uv work
C   files). Usually used after setup by OFCINI.
C   Inputs:
C      TABLE    C*?    Table object name
C      OPCODE   C*4    Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table (must have been
C                               opened with 'WRIT'.
C                      'FLAG' = like 'WRIT' but entry deselected.
C                      'CLOS' = close file, flush on write
C      FCROW    I      Next flag command entry number to read or write.
C   Input/output: (written to or read from FC file)
C   In/out: (refer to specific uv data to be flagged)
C      FLGTIM   R(2)     time range (days)
C      FLGANT   I(2)     antenna 1 (0 -> all), 2 (0 -> all w 1)
C      FLGSOR   I        source number (0 -> all)
C      FLGCHN   I(2)     channel number range
C      FLGIF    I(2)     IF number range
C      FLGSTK   C*4      Stokes flag pattern
C      FLGSUB   I        Subarray
C      FLGFQ    I        FQ id number
C      FLGREA   C*24     User reason for flag
C   In/out: (refer to general flag command in EDITA...)
C      FLGNUM   I        FC command number
C      FLGOP    C*8      flagging operation type
C      FLGIT    I(2)     Time range in editor pixels on "time" axis
C      DTYPE    C*8      Type of data referenced in FLGOP
C      DTIMES   R(2)     Time range referenced in FLGOP
C      DFLUXS   R(2)     Flux (or phase, Tsys, ..) range in FLGOP
C   Output:
C      FCROW    I      Next scan number.
C      IERR     I      Error code, 0=>OK else TABFLG error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, FLGOP*8, FLGSTK*4, DTYPE*8,
     *   FLGREA*24
      INTEGER   FCROW, FLGNUM, FLGANT(2), FLGSOR, FLGCHN(2), FLGIF(2),
     *   FLGSUB, FLGFQ, FLGIT(2), IERR
      REAL      FLGTIM(2), DTIMES(2), DFLUXS(2)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABFC (OPCODE, OBUFFR(OFF,BUFNO), FCROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), FLGTIM, FLGANT, FLGSOR, FLGCHN, FLGIF, FLGSTK,
     *   FLGSUB, FLGFQ, FLGNUM, FLGOP, FLGIT, DTYPE, DTIMES, DFLUXS,
     *   FLGREA, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OFGINI (TABLE, OPCODE, FGROW, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes FLAG extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Output:
C     FGROW       I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else FLGINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   FGROW, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'FG') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT FG'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL FLGINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, FGROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, FGROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABFG (TABLE, OPCODE, FGROW, SOURID, SUBA, FREQID,
     *   ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IERR)
C-----------------------------------------------------------------------
C   Does I/O to FLAG (FG) extension tables. Usually used after setup by
C   OFGINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table (must have been
C                      opened with 'WRIT'.
C                      'FLAG' = like 'WRIT' but entry deselected.
C                      'CLOS' = close file, flush on write
C     FGROW       I   Next FLAG entry number to read or write.
C   Input/output: (written to or read from FLAG file)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     SUBA         I   Subarray number.
C     FREQID       I   Freq. ID number
C     ANTS(2)      I   Antenna numbers, 0=>all
C     TIMER(2)     R   Start and end time of data to be flagged (Days)
C     IFS(2)       I   First and last IF numbers to flag. 0=>all
C     CHANS(2)     I   First and last channel numbers to flag. 0=>all
C     PFLAGS(4)    L   Polarization flags, same order as in data.
C                      .TRUE. => polarization flagged.
C     REASON       C*24 Reason for flagging
C   Output:
C     FGROW        I   Next scan number.
C     IERR         I   Error code, 0=>OK else TABFLG error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, REASON*24
      INTEGER   FGROW, SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2),
     *   IERR
      REAL      TIMER(2)
      LOGICAL   PFLAGS(4)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABFLG (OPCODE, OBUFFR(OFF,BUFNO), FGROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),
     *   SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OFQINI (TABLE, OPCODE, FQROW, NUMIF, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes frequency (FQ) extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Input/output
C     NUMIF        I    Number of IFs
C   Output:
C     FQROW       I   Next row number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else FQINI
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   FQROW, NUMIF, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'FQ') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT FQ'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL FQINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, FQROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), NUMIF, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, FQROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABFQ (TABLE, OPCODE, FQROW, NUMIF, FQID, IFFREQ,
     *   IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
C-----------------------------------------------------------------------
C   Does I/O to frequency (FQ) extension tables. Usually used after
C   setup by OFQINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     FQROW        I   Next entry number to read or write.
C     NUMIF        I   Number of IF's
C   Input/output: (written to or read from frequency table)
C     FQID         I   Frequency ID number, is random parameter
C                      in uv-data.
C     IFFREQ(*)    D   Reference frequency for each IF (Hz)
C     IFCHW(*)     R   Bandwidth of an individual channel (Hz)
C                      Now always written and read as a signed value.
C     IFTBW(*)     R   Total bandwidth of the IF (Hz).  Now written
C                      and read as an unsigned value.
C     IFSIDE(*)    I   Sideband of the IF (-1 => lower, +1 => upper)
C                      Now always written and read as +1
C     BNDCOD       C*8(*)  band/receiver code
C   Output:
C     FQROW        I   Next row number.
C     IERR         I   Error code, 0=>OK else TABFQ error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, BNDCOD(*)*(*)
      INTEGER   FQROW, NUMIF, FQID, IFSIDE(*), IERR
      REAL      IFCHW(*), IFTBW(*)
      DOUBLE PRECISION IFFREQ(*)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABFQ (OPCODE, OBUFFR(OFF,BUFNO), FQROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),
     *   NUMIF, FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCHNDA (TABLE, OPCODE, NIF, FOFF, ISBAND, FINC,
     *   BNDCOD, FREQID, IERR)
C-----------------------------------------------------------------------
C   Routine to create/fill/read CH/FQ extension tables.  TABLE need not
C   be an FQ/CH table but the related one is accessed.
C   Inputs:
C      TABLE    C*?      Table object name
C      OPCODE   C*4      Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      FREQID   I        Frequency ID #, if FQ tables exists.
C                        If OPCODE='READ' and FREQID .le. 0, then
C                        if there is only one row in the FQ table,
C                        that row is returned; if there are multiple
C                        rows an error message is returned.
C   Input/Output:
C      NIF      I        Number of IFs.
C      FOFF     D(*)     Frequency offset in Hz from ref. freq.
C                           True = reference + offset.
C      ISBAND   I(*)     Sideband of each IF.
C                        -1 => 0 video freq. is high freq. end
C                         1 => 0 video freq. is low freq. end
C      FINC     R(*)     Channel bandwidth in Hz of each IF
C   Output:
C      IERR     I        Return error code, 0=>OK, else CHNDAT
C                        error, -1 => tried to create/write an FQ table
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, BNDCOD(*)*(*)
      INTEGER   FREQID, NIF, ISBAND(*), IERR
      REAL      FINC(*)
      DOUBLE PRECISION  FOFF(*)
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN
      CHARACTER TTYPE*2, TMPTAB*36
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Shallow copy of object
      TMPTAB = 'Temporary table for OCHNDA'
      CALL OCOPY(TABLE, TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Look up table info
      CALL TBLKUP (TMPTAB, TDISK, TCNO, TTYPE, TVER, IERR)
      TVER = 1
      IF (IERR.NE.0) GO TO 900
C                                       Open object (assign buffer)
      CALL OBOPEN (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TMPTAB, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TMPTAB, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        Transfer
      CALL CHNDAT (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Delete temp object
      CALL DESTRY (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'OCHNDA: PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OCHNCO (TABLEI, TABLEO, BIF, EIF, NAVG, FREQID, SFOFF,
     *   IERR)
C-----------------------------------------------------------------------
C   Routine to copy a selected portion of an FQ/CH table. TABLEs need
C   not be FQ/CH tables but the related ones are accessed.
C   Inputs:
C      TABLEI   C*?      Input table object name
C      TABLEO   C*?      Output table object name
C      BIF      I        Lowest IF
C      EIF      I        Highest IF
C      NAVG     I        Average by NAVG channels on output
C      FREQID   I        Frequency ID #, if FQ tables exists.
C                        If OPCODE='READ' and FREQID .le. 0, then
C                        if there is only one row in the FQ table,
C                        that row is returned; if there are multiple
C                        rows an error message is returned.
C      SFOFF    D(*)     Source-based frequency offsets per IF
C   Output:
C      IERR     I        Return error code, 0=>OK, else CHNDAT
C                        error, -1 => tried to create/write an FQ table
C-----------------------------------------------------------------------
      CHARACTER TABLEI*(*), TABLEO*(*)
      INTEGER   BIF, EIF, NAVG, FREQID, IERR
      DOUBLE PRECISION SFOFF(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      DOUBLE PRECISION  FOFF(MAXIF)
      INTEGER   TDISK1, TCNO1, TVER1, BUFNO, LUN1, TDISK2, TCNO2,
     *   TVER2, LUN2, CAT2(256)
      CHARACTER TTYPE*2
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look up table info
      CALL TBLKUP (TABLEI, TDISK1, TCNO1, TTYPE, TVER1, IERR)
      TVER1 = 1
      IF (IERR.NE.0) GO TO 900
      CALL TBLKUP (TABLEO, TDISK2, TCNO2, TTYPE, TVER2, IERR)
      TVER2 = 1
      IF (IERR.NE.0) GO TO 900
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLEI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLEI, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUNs
      CALL OBLUN (LUN1, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL OBLUN (LUN2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLKs
      CALL OBHGET (TABLEI, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL OBHGET (TABLEO, CAT2, IERR)
      IF (IERR.NE.0) GO TO 900

C                                        Copy
      CALL CHNNCP (TVER1, TVER2, LUN1, LUN2, TDISK1, TDISK2, TCNO1,
     *   TCNO2, CATBLK, CAT2, BIF, EIF, NAVG, FREQID, SFOFF,
     *   OBUFFR(1,BUFNO), FOFF, ISBAND, FINC, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save output catblk
      CALL OBHPUT (TABLEO, CAT2, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLEI, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Free LUN
      CALL OBLUFR (LUN1)
      CALL OBLUFR (LUN2)
      GO TO 999
C                                       Error
 900  MSGTXT = 'OCHNCO: PROBLEM WITH TABLE OBJECT ' // TABLEI
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OGCINI (TABLE, OPCODE, GCROW, NUMPOL, NUMIF, NUMTAB,
     *                   IRET)
C-----------------------------------------------------------------------
C   Open a GC table.
C
C   If TABLE references an existing GC table and OPCODE is 'READ' then
C   open the table for reading, set GCROW to 1, set NUMIF to the value
C   of the NO_IF keyword in the table header, and set IRET to zero.
C
C   If TABLE references an existing GC table and OPCODE is 'WRIT' then
C   open the table for reading and writing, set GCROW to one more than
C   the number of rows in the table, set NUMIF to the value of the
C   NO_IF keyword in the table header, and set IRET to zero.
C
C   If TABLE references a GC table that does not exist and OPCODE is
C   'WRIT' then create the GC table, open it for reading and writing,
C   set the value of the NO_IF keyword in the table header to NUMIF,
C   set GCROW to 1, and set IRET to zero.
C
C   If TABLE does not reference a GC table, OPCODE is not 'READ' or
C   'WRIT', or if it is not possible to open the table then issue
C   one or more error messages and set IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)    name of table object
C      OPCODE  C*4      open mode: 'READ' or 'WRIT'
C
C   Input/Output:
C      NUMPOL  I        number of polarizations covered by table
C      NUMIF   I        number of IFs covered by table
C      NUMTAB  I        maximum number of tabulated entries
C
C   Output:
C      GCROW   I        next row to read or write
C      IRET    I        return status: 0 if table opened,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      CHARACTER OPCODE*4
      INTEGER   GCROW
      INTEGER   NUMPOL
      INTEGER   NUMIF
      INTEGER   NUMTAB
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'TABSTUFF.INC'
C
C     Local variables:
C
C     BUFNUM   allocated buffer number
C     OFFSET   offset of I/O region in allocated buffer
C
C     TDISK    disk number for table
C     TCNO     catalogue number for table
C     TTYPE    table type
C     TVER     table version number
C
C     LUN      LUN allocated for table I/O
C
C     DIM      object attribute dimensions
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   TDISK
      INTEGER   TCNO
      CHARACTER TTYPE*2
      INTEGER   TVER
C
      INTEGER   LUN
C
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve table information and set IRET to 0 or set IRET to a non-
C     zero value if the table information can not be retrieved:
C
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (TTYPE .EQ. 'GC') THEN
C
C           Assign an I/O buffer for the table and set IRET to 0 or set
C           IRET to a non-zero value if no buffers are available:
C
            CALL OBOPEN (TABLE, IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Retrieve the buffer number and set IRET to 0:
C
               CALL OBINFO (TABLE, BUFNUM, IRET)
C
C              Assign a LUN for the table and set IRET to 0 or set IRET
C              to a non-zero value if no LUNs are available:
C
               CALL OBLUN (LUN, IRET)
C
               IF (IRET .EQ. 0) THEN
C
C                 Retrieve the catalogue header blcok for the parent
C                 file and set IRET to 0 or set IRET to a non-zero
C                 value if the catalogue header block can not be
C                 retrieved:
C
                  CALL OBHGET (TABLE, CATBLK, IRET)
C
                  IF (IRET .EQ. 0) THEN
C
C                    Open the table, update TVER to the actual version
C                    number of the table if it was zero before, and set
C                    IRET to 0 or set IRET to a non-zero value if it is
C                    not possible to open the table:
C
                     CALL GCINI (OPCODE, OBUFFR(OFFSET, BUFNUM), TDISK,
     *                           TCNO, TVER, CATBLK, LUN, GCROW,
     *                           TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM),
     *                           NUMPOL, NUMIF, NUMTAB, IRET)
C
                     IF (IRET .EQ. 0) THEN
C
C                       Record the actual version number:
C
                        DIM(1) = 1
                        DIM(2) = 1
                        DIM(3) = 0
                        CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER,
     *                               CDUMMY, IRET)
C
C                       If a new table was created the catalogue header
C                       block is changed and needs to be written back to
C                       disk.
C
                        IF (OPCODE .EQ. 'WRIT') THEN
C
C                          Update the header block and set IRET to 0
C                          or set IRET to a non-zero value if it is not
C                          possible to update the catalogue header:
C
                           CALL OBHPUT (TABLE, CATBLK, IRET)
C
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 7000) IRET
                              CALL MSGWRT (7)
                           END IF
                        END IF
C
C                       The table needs to be closed and re-opened so
C                       that the generic table access routines in APLOOP
C                       will work.
C
                        CALL TABIO ('CLOS', 0, GCROW,
     *                              OBUFFR(OFFSET, BUFNUM),
     *                              OBUFFR(OFFSET, BUFNUM), IRET1)
                        IF (IRET1 .NE. 0) THEN
                           WRITE (MSGTXT, 7001) IRET1
                           CALL MSGWRT (7)
                           IRET = IRET1
                        END IF
                     ELSE
C
C                       Table could not be opened.
C
                        WRITE (MSGTXT, 7002) IRET
                        CALL MSGWRT (7)
                     END IF
                  ELSE
C
C                    Catalogue header could not be retrieved.
C
                     WRITE (MSGTXT, 7003) IRET
                     CALL MSGWRT (7)
                  END IF
C
C                 Free LUN:
C
                  CALL OBLUFR (LUN)
C
               ELSE
C
C                 Unable to allocate LUN.
C
                  WRITE (MSGTXT, 7004) IRET
                  CALL MSGWRT (7)
               END IF
C
C              Free the table buffer and set IRET1 to 0 or set IRET1 to
C              a non-zero value if it is not possible to free the
C              buffer:
C
               CALL OBCLOS (TABLE, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 7005) IRET1
                  CALL MSGWRT (7)
                  IRET = IRET1
               END IF
            ELSE
C
C              Failed to allocate table buffer.
C
               WRITE (MSGTXT, 7006) IRET
               CALL MSGWRT (7)
            END IF
C
C           Re-open the table using the generic APLOOP table interface
C           and set IRET1 to 0 or set IRET1 to a non-zero value if the
C           table can not be re-opened:
C
            CALL TABOPN (TABLE, OPCODE, IRET1)
C
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 7007) IRET1
               CALL MSGWRT (7)
               IRET = IRET1
            END IF
         ELSE
C
C           TABLE does not refer to a GC table.
C
            WRITE (MSGTXT, 7008) TTYPE
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        Failed to retrieve table information.
C
         WRITE (MSGTXT, 7009) IRET
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OGCINI: FAILED TO UPDATE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7001 FORMAT ('OGCINI: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 7002 FORMAT ('OGCINI: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 7003 FORMAT ('OGCINI: FAILED TO RETREIVE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7004 FORMAT ('OGCINI: FAILED TO ALLOCATE LUN (ERROR ', I4, ')')
 7005 FORMAT ('OGCINI: FAILED TO FREE TABLE BUFFER (ERROR ', I4, ')')
 7006 FORMAT ('OGCINI: FAILED TO ALLOCATE TABLE BUFFER (ERROR ', I4,
     *        ')')
 7007 FORMAT ('OGCINI: FAILED TO RE-OPEN TABLE (ERROR ', I4, ')')
 7008 FORMAT ('OGCINI: TABLE MUST HAVE TYPE ''GC'' NOT ''', A2, '''')
 7009 FORMAT ('OGCINI: FAILED TO RETRIEVE TABLE INFORMATION (ERROR ',
     *        I4, ')')
      END
      SUBROUTINE OTABGC (TABLE, OPCODE, GCROW, NUMPOL, NUMTAB, ANTNUM,
     *                   SUBARR, FREQID, GCTYPE, NTERMS, XTYPE, YTYPE,
     *                   XVALUE, YVALUE, RGAIN, SENS, IRET)
C-----------------------------------------------------------------------
C   Read, write, or close GC table.
C
C   If TABLE is an open GC table and OPCODE is 'READ' then set FREQID
C   through OVRLAP from row GCROW of the table, increment GCROW by 1,
C   and set IRET to 0 if row GCROW is not flagged or a negative value if
C   row GCROW is flagged. If TABLE is an open GC table and OPCODE is
C   'READ' and row number GCROW can not be read then issue one or more
C   error messages and set IRET to a positive value.
C
C   If TABLE is an open GC table and OPCODE is 'WRIT' then write the
C   values of FREQID through OVRLAP to row GCROW of the table, increment
C   GCROW by 1, and set IRET to 0. If TABLE is an open GC table and
C   OPCODE is 'WRIT' and row number GCROW can not be written then issue
C   one or more error messages and set IRET to a positive value.
C
C   If TABLE is an open GC table and OPCODE is 'CLOS' then close the
C   table and set IRET to 0. If TABLE is an open GC table and OPCODE is
C   'CLOS' and the table can not be closed then issue one or more error
C   messages and set IRET to a non-zero value.
C
C   If TABLE is not an open table then issue an error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)       name of table object
C      OPCODE  C*4         'READ', 'WRIT', or 'CLOS'
C      NUMPOL   I           number of polarizations
C      NUMTAB   I           dimension for tabulated values
C
C   Input/Output:
C      GCROW   I           row number to read or write; incremented on
C                          output
C      ANTNUM  I           antenna number
C      SUBARR  I           subarray number
C      FREQID  I           frequency ID
C      GCTYPE  I(2, MAXIF) gain curve type for each polarization and IF
C                          1 - tabulated values
C                          2 - polynomial
C                          3 - spherical harmonic
C      NTERMS  I(2, MAXIF) number of tabulated values or terms for each
C                          polarization and IF
C      XTYPE   I(2, MAXIF) x value types for each polarization and IF
C                          0 - none
C                          1 - elevation in degrees
C                          2 - zenith angle in degrees
C                          3 - hour angle in degrees
C                          4 - declination in degrees
C                          5 - codeclination in degrees
C      YTYPE   I(2, MAXIF) y value types for each polarization and IF
C                          1 - elevation in degrees
C                          2 - zenith angle in degrees
C                          3 - hour angle in degrees
C                          4 - declination in degrees
C                          5 - codeclination in degrees
C      XVALUE  R(2, MAXIF) x value for each polarization and IF
C      YVALUE  R(2, MAXIF, NUMTAB)
C                          tabulated y values for each polarization and
C                          IF
C      RGAIN   R(2, MAXIF, NUMTAB)
C                          tabulated gains for each polarization and IF
C      SENS    R(2, MAXIF) sensitivity for each polarization and IF in
C                          kelvin per jansky
C
C   Output:
C      IRET    I           return status: 0 if operation complete
C                                         non-zero if operation failed
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER TABLE*(*)
      CHARACTER OPCODE*4
      INTEGER GCROW
      INTEGER NUMPOL
      INTEGER NUMTAB
      INTEGER ANTNUM
      INTEGER SUBARR
      INTEGER FREQID
      INTEGER GCTYPE(2, MAXIF)
      INTEGER NTERMS(2, MAXIF)
      INTEGER XTYPE(2, MAXIF)
      INTEGER YTYPE(2, MAXIF)
      REAL    XVALUE(2, MAXIF)
      REAL    YVALUE(2, MAXIF, NUMTAB)
      REAL    RGAIN(2, MAXIF, NUMTAB)
      REAL    SENS(2, MAXIF)
      INTEGER IRET
C
      INCLUDE 'TABSTUFF.INC'
C
C     Local Variables:
C
C     BUFNUM      number of buffer allocated for table I/O; zero if
C                 table is not open
C     OFFSET      offset to I/O region in allocated buffer
C
C     IRET1       alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve buffer number for TABLE and set IRET to 0 or set IRET to
C     a non-zero value if TABLE is not a defined object:
C
      CALL OBINFO (TABLE, BUFNUM, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (BUFNUM .GT. 0) THEN
C
C           Perform the operation and set IRET accordingly:
C
            CALL TABGC (OPCODE, OBUFFR(OFFSET, BUFNUM), GCROW,
     *                  TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM), NUMPOL,
     *                  NUMTAB, ANTNUM, SUBARR, FREQID, GCTYPE, NTERMS,
     *                  XTYPE, YTYPE, XVALUE, YVALUE, RGAIN, SENS, IRET)
            IF (IRET .GT. 0) THEN
               WRITE (MSGTXT, 7000) IRET
               CALL MSGWRT (7)
            END IF
C
C           The table buffer needs to be deallocated when the table is
C           closed.
C
            IF (OPCODE .EQ. 'CLOS') THEN
               CALL OBCLOS (TABLE, IRET1)
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 7001) IRET
                  CALL MSGWRT (7)
               END IF
            END IF
         ELSE
C
C           Table is not open.
C
            WRITE (MSGTXT, 7002)
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        TABLE does not name a recognized table object.
C
         WRITE (MSGTXT, 7003)
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OTABGC: TABLE I/O FAILED (ERROR ', I4, ')')
 7001 FORMAT ('OTABGC: FAILED TO DE-ALLOCATE BUFFER (ERROR ', I4, ')')
 7002 FORMAT ('OTABGC: TABLE IS NOT OPEN')
 7003 FORMAT ('OTABGC: TABLE OBJECT IS NOT DEFINED')
      END
      SUBROUTINE OGPINI (TABLE, OPCODE, GPROW, RCVR, RLONG, RLAT, RHT,
     *   IERR)
C-----------------------------------------------------------------------
C   Inputs:
C    TABLE    C*8           Table object name
C    OPCODE   C*4           Operation code:
C                            'WRIT' - open for reading and writing,
C                                     create if necessary
C                            'READ' - open for reading only
C   Input/output:
C    RCVR     C*(*)         Receiver name
C    RLONG    R             East longitude of receiver (degrees)
C    RLAT     R             Latitude of receiver (degrees)
C    RHT      R             Height of receiver above MSL (metres)
C   Output:
C    GPROW    I             Next record to read or write: 1 if OPCODE
C                            is 'READ', one greater than the current
C                            number of records if OPCODE is 'WRIT'
C    IERR     I             Return error code: 0 - OK
C                                              1 - error
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, RCVR*8
      INTEGER   GPROW, IERR
      REAL      RLONG, RLAT, RHT
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'GP') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT GP'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C
      CALL GPINI (OPCODE, OBUFFR(OFF, BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, GPROW, TBKOLS(1, BUFNO), TBNUMV(1, BUFNO), RCVR, RLONG,
     *   RLAT, RHT, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, GPROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABGP (TABLE, OPCODE, GPROW, TIME, PRN, AZ, EL,
     *   TECTAU, TECPHS, IERR)
C-----------------------------------------------------------------------
C   Does I/O to GPS (GP) extention tables. Usually used after setup by
C   OGPINI.
C   Inputs:
C      TABLE        C*? Table object name
C      OPCODE       C*4 Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C   Input/output: (written to or read from baseline file)
C      GPROW        I   Next entry number to read or write.
C      TIME         D   Time of measurement (days)
C      PRN          I   Satellite PRN (id number)
C      AZ           R   Satellite azimuth (deg)
C      EL           R   Satellite elevation (deg)
C      TECTAU       R   TEC along the line of sight derived from delay
C                       (electrons/m^2)
C      TECPHS       R   TEC along the line of sight derived from phase
C                       (electrons/m^2)
C   Output:
C      IERR         I   Error code, 0=>OK else TABGP error.
C                       Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   GPROW, PRN, IERR
      DOUBLE PRECISION TIME
      REAL      AZ, EL, TECTAU, TECPHS
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 900
         END IF
C                                        Transfer
      CALL TABGP (OPCODE, OBUFFR(OFF,BUFNO), GPROW, TBKOLS(1,BUFNO),
     *   TIME, PRN, AZ, EL, TECTAU, TECPHS, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OHFINI (TABLE, OPCODE, HFROW, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes MkIII Haystack FRNGE (fringe fitting) table.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Output:
C     HFROW        I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else HFINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   HFROW, IERR
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'HF') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT HF'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL HFINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, HFROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, HFROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C-----------------------------------------------------------------------
C
 999  RETURN
      END
      SUBROUTINE OTABHF (TABLE, OPCODE, HFROW,
     *   CC1, IC2, IC3, IC4, IC5, IC6, IC7, IC8, IC9, IC10, IC11, IC12,
     *   IC13, IC14, IC15, IC16, IC17, IC18, IC19, IC20, IC21, IC22,
     *   IC23, IC24, CC25, CC26, CC27, CC28, CC29, CC30, CC31, CC32,
     *   CC33, CC34, CC35, CC36, CC37, CC38, DC39, DC40, DC41, DC42,
     *   DC43, DC44, DC45, DC46, DC47, DC48, DC49, DC50, RC51, RC52,
     *   RC53, RC54, RC55, RC56, RC57, RC58, RC59, RC60, RC61, RC62,
     *   RC63, RC64, RC65, RC66, RC67, RC68, RC69, RC70, RC71, RC72,
     *   RC73, RC74, RC75, RC76, RC77,
     *   IERR)
C-----------------------------------------------------------------------
C   Does I/O to MkIII FRNGE (VLBI fringe fitting) tables. Usually used
C   after setup by OHFINI.
C   Inputs:
C     TABLE    C*?     Table object name
C     OPCODE   C*4     Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     HFROW   I       Next scan number to read or write.
C   Input/output: (written to or read from HF file)
C       CC1   C*2   Baseline code
C       IC2   I(6)  UTC time tag (YMDHMS) EPOCH for delay and rate
C       IC3   I(6)  UTC of FRNGE processing (YMDHMS)
C       IC4   I(4)  ARCHIV
C       IC5   I     Sample rate in kbits/sec
C       IC6   I     no. frames per parameter period
C       IC7   I     Pass number
C       IC8   I     #U/L sideband pairs included in this processing
C                   (i.e. # of discrete LO frequencies)
C       IC9   I(28) #Accumulation periods by sideband by channel
C       IC10  I(56) (2,2,14)  Track # table in order of:
C                      Station #
C                      Refer/partn Sideband
C                      Channel #
C       IC11  I     COREL version #
C       IC12  I(6)  UTC time tag (YMDHMS)  EPOCH for "central" EPOCH
C       IC13  I     LU of printout (for FRNGP)
C       IC14  I     Ref. tape drive number
C       IC15  I     Rem. tape drive number
C       IC16  I     Special options NAME(211)
C       IC17  I     Integer value of PARAM(250) [=999 for special PCAL
C                   by AP]
C       IC18  I(28) CORELXTNT(2,14)  COREL extent# from which each track
C                   is taken.
C       IC19  I(84) CALBYFRQ(3,2,14) Phase-cal amp, phase and freq by
C                   station and channel normalized as follows:
C                      AMP 0 to 10000(in voltage) -1=manual cal
C                      PHS -18000 to +18000
C                      frq  kHz
C       IC20  I(28) PROCUTC(2,14) COREL processing UT (YDDD) by sideband
C                   and channel.
C       IC21  I(56) ERRORATE(2,2,14) Tape error rate by station,
C                   sideband and channel encoded as:
C                      1000*LOGT(error rate)
C                      (=-32000 if no errors)
C       IC22  I(28) INDEX(2,14) COREL index #s by sideband and FRNGE CH#
C       IC23  I     FRNGE error code 0=OK NE.0 Don't use
C       IC24  I     SBDOFFST flag 1=this run had sideband fixup
C       CC25  C*8   STAR ID, Radio source name
C       CC26  C*8   First antenna name of baseline
C       CC27  C*8   Second antenna of baseline
C       CC28  C*6   CORELFILE COREL correlation output file name
C       CC29  C*8   first raw-data tape ID label
C       CC30  C*8   second raw-data tape ID label
C       CC31  C*6   VLB2PRG FRNGE program version YYMMDD
C       CC32  C*8   RUN CODE  Run code, e.g. "329-1300"
C       CC33  C*1   FRNGE quality code 0=no good, 1=very poor, 9=very
C                   good, A=has FRNGE error code=1,B=2 etc.
C       CC34  C*2   Frequency group code
C       CC35  C*6   Original COREL file name
C       CC36  C*6   Tape Q code
C       CC37  C*8   Ref station occupation code
C       CC38  C*8   Rem station occupation code
C       DC39  D(14) RFREQ(14) LO frequencies (MHz) by channel
C       DC40  D     REF FREQ RF freq (MHz) to which phase is referred
C       DC41  D     DEL OBSV Observed group delay in microseconds
C                   equals single band delay if only one freq.
C                   processed.  DELOBSV=T2=T1, T1=time of arrival at
C                   site 1 (reference site) as measured by the site
C                   clock at site 1, T2=time of signal arrival at site 2
C                   as measured by site clock at site 2.  The signal in
C                   question is the one which arrives at site 1 at a UTC
C                   time equal to that given as the UTC epoch.
C       DC42  D     RAT OBSV  Observed delay rate (usec/sec) corrected
C                   for the phase cal rate.
C       DC43  D     NB DELAY Narrow-band group-delay (usec)
C       DC44  D     DGPD group delay ambig. (usec)
C       DC45  D     BTE0 Apriori clock (usec)
C       DC46  D     EPOCH0  ref. st. clock epoch (usec)
C       DC47  D     DEL OBSVM observed delay at central epoch
C       DC48  D     RAT OBSVM observed delay rate at central epoch
C       DC49  D     DLY2 phase delay at EPO+1 sec
C       DC50  D     DLY3 phase delay at EPO-1 sec.
C       RC51  R(28) AMBYFRQ(2,14) Amp and phase by channel 1=100%
C                   phase=-180 to 180 deg. residual to COREL and
C                   uncorrected for PCAL rate.
C       RC52  R(2)  Phase cal rate by station (usec/sec)
C       RC53  R     DELRESID Delay residual to COREL a priori.
C       RC54  R     DELSIGMA Calculated delay error (usec)
C       RC55  R     RATRESID Delay-rate residual to COREL a priori
C                   (usec/sec) corrected for phase cal rates.
C       RC56  R     RATSIGMA Calculate delay-rate error (usec/sec)
C       RC57  R     COHERCOR Coherent multi-freq correlation
C                   coefficient. (1=100% correlation)
C       RC58  R     TOTPHASE Total observed fringe phase (deg)
C       RC59  R(2)  UVF/ASEC Fringes per asec in N-S and E-W
C       RC60  R(2)  STARELEV Calculated star elevations run reference
C                   time by station (deg).
C       RC61  R     AAMP FRNGE amplitude fro incoherent addition of
C                   frequency channels.
C       RC62  R(2)  URVRSEC Rate derivatives mHz/sec arc
C       RC63  R(6)  SRCHPAR Search parameters
C       RC64  R     DEPSBRES Single band delay residual (usec)
C       RC65  R     SNR Signal to noise ratio in sigma.
C       RC66  R     PROB Probability of a false detection.
C       RC67  R     INCOH Incoherent segmented fringe amp. in units of
C                   10,000.
C       RC68  R     EARP total phase refered to an epoch at time the
C                   signal reaches the center of teh earth.
C       RC69  R     REARP Residual phase corrected to earth centered
C                   epoch
C       RC70  R     START Start time in seconds past hour
C       RC71  R     STOP Stop time in seconds past hour
C       RC72  R     EPD Epoch offset from center of run in sec.
C       RC73  R     DUR Effective run duration in seconds.
C       RC74  R     DELSS Single-band delay error in microsec.
C       RC75  R     QB Ratio of min to max data accepted in %
C       RC76  R     DISCD % data discarded
C       RC77  R     TOTPM Total phase at central epoch
C    Output:
C       HFROW I      Next row number.
C       IERR   I      Error code, 0=>OK else TABHF error.
C                    Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4,
     *   CC1*2, CC25*8, CC26*8, CC27*8, CC28*6, CC29*8, CC30*8, CC31*6,
     *   CC32*8, CC33*1, CC34*2, CC35*6, CC36*6, CC37*8, CC38*8
      INTEGER   HFROW, IERR,
     *   IC2(6), IC3(6), IC4(4), IC5, IC6, IC7, IC8, IC9(28), IC10(56),
     *   IC11, IC12(6), IC13, IC14, IC15, IC16, IC17, IC18(28),
     *   IC19(84), IC20(28), IC21(56), IC22(28), IC23, IC24
      REAL      RC51(28), RC52(2), RC53, RC54, RC55, RC56, RC57, RC58,
     *   RC59(2), RC60(2), RC61, RC62(2), RC63(6), RC64, RC65, RC66,
     *   RC67, RC68, RC69, RC70, RC71, RC72, RC73, RC74, RC75, RC76,
     *   RC77
      DOUBLE PRECISION DC39(14), DC40, DC41, DC42, DC43, DC44, DC45,
     *   DC46, DC47, DC48, DC49, DC50
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABHF (OPCODE, OBUFFR(OFF,BUFNO), HFROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),
     *   CC1, IC2, IC3, IC4, IC5, IC6, IC7, IC8, IC9, IC10, IC11, IC12,
     *   IC13, IC14, IC15, IC16, IC17, IC18, IC19, IC20, IC21, IC22,
     *   IC23, IC24, CC25, CC26, CC27, CC28, CC29, CC30, CC31, CC32,
     *   CC33, CC34, CC35, CC36, CC37, CC38, DC39, DC40, DC41, DC42,
     *   DC43, DC44, DC45, DC46, DC47, DC48, DC49, DC50, RC51, RC52,
     *   RC53, RC54, RC55, RC56, RC57, RC58, RC59, RC60, RC61, RC62,
     *   RC63, RC64, RC65, RC66, RC67, RC68, RC69, RC70, RC71, RC72,
     *   RC73, RC74, RC75, RC76, RC77, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OIMINT (TABLE, OPCODE, IMROW, OBSCOD, RDATE, NUMSTK,
     *                   STK1, NUMIF, NUMCHN, REFFRQ, CHANBW, REFPIX,
     *                   NUMPOL, NUMPLY, CORREV, IRET)
C-----------------------------------------------------------------------
C   Open an interferometer model (IM) table.
C
C   If TABLE references an existing IM table and OPCODE is 'READ' then
C   open the table for reading, set IMROW to 1, set OBSCOD through
C   CORREV from the table header, and set IRET to zero.
C
C   If TABLE references an existing IM table and OPCODE is 'WRIT' then
C   open the table for reading and writing, set IMROW to one more than
C   the number of rows in the table, set set OBSCOD through CORREV from
C   the table header, and set IRET to zero.
C
C   If TABLE references an IM table that does not exist and OPCODE is
C   'WRIT' then create the IM table, open it for reading and writing,
C   fill the header from OBSCOD through CORREV, set IMROW to 1, and set
C   IRET to zero.
C
C   If TABLE does not reference a IM table, OPCODE is not 'READ' or
C   'WRIT', or if it is not possible to open the table then issue
C   one or more error messages and set IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)    name of table object
C      OPCODE  C*4      open mode: 'READ' or 'WRIT'
C
C   Input/Output:
C      OBSCOD  C*8      observing code
C      RDATE   C*8      reference data (YYYYMMDD)
C      NUMSTK  I        size of STOKES axis in parent data file
C      STK1    I        reference value for STOKES axis in parent data
C                       file
C      NUMIF   I        size of IF axis in parent data file
C      NUMCHN  I        size of FREQ axis in parent data file
C      REFFRQ  I        reference frequency in Hz
C      CHANBW  I        channel spacing in Hz
C      REFPIX  I        reference pixel number for FREQ axis
C      NUMPOL  I        number of polarizations in table (1 or 2)
C      NUMPLY  I        number of polynomial terms used in table
C      CORREV  I        revision number of correlator software that
C                       generated the original table
C
C   Output:
C      IMROW   I        next row to read or write
C      IRET    I        return status: 0 if table opened,
C                                      non-zero otherwise
C
C   Note:
C      A similar routine, OIMINI, uses a common block rather than an
C      argument list. This interface is preferred.
C-----------------------------------------------------------------------
      CHARACTER        TABLE*(*)
      CHARACTER        OPCODE*4
      INTEGER          IMROW
      CHARACTER        OBSCOD*8
      CHARACTER        RDATE*8
      INTEGER          NUMSTK
      INTEGER          STK1
      INTEGER          NUMIF
      INTEGER          NUMCHN
      DOUBLE PRECISION REFFRQ
      DOUBLE PRECISION CHANBW
      DOUBLE PRECISION REFPIX
      INTEGER          NUMPOL
      INTEGER          NUMPLY
      DOUBLE PRECISION CORREV
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'TABSTUFF.INC'
C
C     Local variables:
C
C     BUFNUM   allocated buffer number
C     OFFSET   offset of I/O region in allocated buffer
C
C     TDISK    disk number for table
C     TCNO     catalogue number for table
C     TTYPE    table type
C     TVER     table version number
C
C     LUN      LUN allocated for table I/O
C
C     DIM      object attribute dimensions
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   TDISK
      INTEGER   TCNO
      CHARACTER TTYPE*2
      INTEGER   TVER
C
      INTEGER   LUN
C
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve table information and set IRET to 0 or set IRET to a non-
C     zero value if the table information can not be retrieved:
C
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (TTYPE .EQ. 'IM') THEN
C
C           Assign an I/O buffer for the table and set IRET to 0 or set
C           IRET to a non-zero value if no buffers are available:
C
            CALL OBOPEN (TABLE, IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Retrieve the buffer number and set IRET to 0:
C
               CALL OBINFO (TABLE, BUFNUM, IRET)
C
C              Assign a LUN for the table and set IRET to 0 or set IRET
C              to a non-zero value if no LUNs are available:
C
               CALL OBLUN (LUN, IRET)
C
               IF (IRET .EQ. 0) THEN
C
C                 Retrieve the catalogue header blcok for the parent
C                 file and set IRET to 0 or set IRET to a non-zero
C                 value if the catalogue header block can not be
C                 retrieved:
C
                  CALL OBHGET (TABLE, CATBLK, IRET)
C
                  IF (IRET .EQ. 0) THEN
C
C                    Open the table, update TVER to the actual version
C                    number of the table if it was zero before, and set
C                    IRET to 0 or set IRET to a non-zero value if it is
C                    not possible to open the table:
C
                     CALL IMINIT (OPCODE, OBUFFR(OFFSET, BUFNUM), TDISK,
     *                            TCNO, TVER, CATBLK, LUN, IMROW,
     *                            TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM),
     *                            OBSCOD, RDATE, NUMSTK, STK1, NUMIF,
     *                            NUMCHN, REFFRQ, CHANBW, REFPIX,
     *                            NUMPOL, NUMPLY, CORREV, IRET)
C
                     IF (IRET .EQ. 0) THEN
C
C                       Record the actual version number:
C
                        DIM(1) = 1
                        DIM(2) = 1
                        DIM(3) = 0
                        CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER,
     *                               CDUMMY, IRET)
C
C                       If a new table was created the catalogue header
C                       block is changed and needs to be written back to
C                       disk.
C
                        IF (OPCODE .EQ. 'WRIT') THEN
C
C                          Update the header block and set IRET to 0
C                          or set IRET to a non-zero value if it is not
C                          possible to update the catalogue header:
C
                           CALL OBHPUT (TABLE, CATBLK, IRET)
C
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 7000) IRET
                              CALL MSGWRT (7)
                           END IF
                        END IF
C
C                       The table needs to be closed and re-opened so
C                       that the generic table access routines in APLOOP
C                       will work.
C
                        CALL TABIO ('CLOS', 0, IMROW,
     *                              OBUFFR(OFFSET, BUFNUM),
     *                              OBUFFR(OFFSET, BUFNUM), IRET1)
                        IF (IRET1 .NE. 0) THEN
                           WRITE (MSGTXT, 7001) IRET1
                           CALL MSGWRT (7)
                           IRET = IRET1
                        END IF
                     ELSE
C
C                       Table could not be opened.
C
                        WRITE (MSGTXT, 7002) IRET
                        CALL MSGWRT (7)
                     END IF
                  ELSE
C
C                    Catalogue header could not be retrieved.
C
                     WRITE (MSGTXT, 7003) IRET
                     CALL MSGWRT (7)
                  END IF
C
C                 Free LUN:
C
                  CALL OBLUFR (LUN)
C
               ELSE
C
C                 Unable to allocate LUN.
C
                  WRITE (MSGTXT, 7004) IRET
                  CALL MSGWRT (7)
               END IF
C
C              Free the table buffer and set IRET1 to 0 or set IRET1 to
C              a non-zero value if it is not possible to free the
C              buffer:
C
               CALL OBCLOS (TABLE, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 7005) IRET1
                  CALL MSGWRT (7)
                  IRET = IRET1
               END IF
            ELSE
C
C              Failed to allocate table buffer.
C
               WRITE (MSGTXT, 7006) IRET
               CALL MSGWRT (7)
            END IF
C
C           Re-open the table using the generic APLOOP table interface
C           and set IRET1 to 0 or set IRET1 to a non-zero value if the
C           table can not be re-opened:
C
            CALL TABOPN (TABLE, OPCODE, IRET1)
C
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 7007) IRET1
               CALL MSGWRT (7)
               IRET = IRET1
            END IF
         ELSE
C
C           TABLE does not refer to an IM table.
C
            WRITE (MSGTXT, 7008) TTYPE
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        Failed to retrieve table information.
C
         WRITE (MSGTXT, 7009) IRET
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OIMINT: FAILED TO UPDATE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7001 FORMAT ('OIMINT: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 7002 FORMAT ('OIMINT: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 7003 FORMAT ('OIMINT: FAILED TO RETREIVE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7004 FORMAT ('OIMINT: FAILED TO ALLOCATE LUN (ERROR ', I4, ')')
 7005 FORMAT ('OIMINT: FAILED TO FREE TABLE BUFFER (ERROR ', I4, ')')
 7006 FORMAT ('OIMINT: FAILED TO ALLOCATE TABLE BUFFER (ERROR ', I4,
     *        ')')
 7007 FORMAT ('OIMINT: FAILED TO RE-OPEN TABLE (ERROR ', I4, ')')
 7008 FORMAT ('OIMINT: TABLE MUST HAVE TYPE ''IM'' NOT ''', A2, '''')
 7009 FORMAT ('OIMINT: FAILED TO RETRIEVE TABLE INFORMATION (ERROR ',
     *        I4, ')')
      END
      SUBROUTINE OTABIM (TABLE, OPCODE, IMROW, NUMPOL, TIME, TIMINT,
     *                   SOURID, ANTNUM, SUBARR, FREQID, IFR, FREQVR,
     *                   PDELAY, GDELAY, PRATE, GRATE, DISP, DDISP,
     *                   IRET)
C-----------------------------------------------------------------------
C   Read, write, or close an interferometer model (IM) table.
C
C   If TABLE is an open IM table and OPCODE is 'READ' then set TIME
C   through DDISP from row IMROW of the table, increment IMROW by 1,
C   and set IRET to 0 if row IMROW is not flagged or a negative value if
C   row IMROW is flagged. If TABLE is an open IM table and OPCODE is
C   'READ' and row number IMROW can not be read then issue one or more
C   error messages and set IRET to a positive value.
C
C   If TABLE is an open IM table and OPCODE is 'WRIT' then write the
C   values of TIME through DDISP to row IMROW of the table, increment
C   IMROW by 1, and set IRET to 0. If TABLE is an open IM table and
C   OPCODE is 'WRIT' and row number IMROW can not be written then issue
C   one or more error messages and set IRET to a positive value.
C
C   If TABLE is an open IM table and OPCODE is 'CLOS' then close the
C   table and set IRET to 0. If TABLE is an open IM table and OPCODE is
C   'CLOS' and the table can not be closed then issue one or more error
C   messages and set IRET to a non-zero value.
C
C   If TABLE is not an open table then issue an error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)       name of table object
C      OPCODE  C*4         'READ', 'WRIT', or 'CLOS'
C      NUMPOL   I           number of polarizations in table (1 or 2)
C
C   Input/Output:
C      IMROW   I              row number to read or write; incremented
C                             on successful read or write
C      TIME    D              time in days
C      TIMINT  R              time interval in days
C      SOURID  I              source ID number
C      ANTNUM  I              antenna number
C      SUBARR  I              subarray number
C      FREQID  I              frequency ID number
C      IFR     R              ionospheric Faraday rotation in radians
C                             per meter squared
C      FREQVR  R(*)           time-variable frequency offset for each IF
C                             in Hz
C      PDELAY  D(2, MAXIF, *) phase delay polynomials for each
C                             polarization and IF giving delay in
C                             turns
C      GDELAY  D(2, *)        group delay polynomials for each
C                             polarization giving delay in seconds
C      PRATE   D(2, MAXIF, *) phase rate polynomials for each
C                             polarization and IF giving rate in Hz
C      GRATE   D(2, *)        group rate polynomials for each
C                             polarization giving rate in seconds per
C                             second
C      DISP    R              dispersive delay in seconds at 1 m
C                             wavelength
C      DDISP   R              rate of change of dispersive delay at 1 m
C                             in seconds per second
C
C   Output:
C      IRET    I           return status: negative if a flagged row was
C                                         read, 0 if operation complete,
C                                         non-zero if operation failed
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER        TABLE*(*)
      CHARACTER        OPCODE*4
      INTEGER          IMROW
      INTEGER          NUMPOL
      DOUBLE PRECISION TIME
      REAL             TIMINT
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      REAL             IFR
      REAL             FREQVR(*)
      DOUBLE PRECISION PDELAY(2, MAXIF, *)
      DOUBLE PRECISION GDELAY(2, *)
      DOUBLE PRECISION PRATE(2, MAXIF, *)
      DOUBLE PRECISION GRATE(2, *)
      REAL             DISP
      REAL             DDISP
      INTEGER          IRET
C
      INCLUDE 'TABSTUFF.INC'
C
C     Local Variables:
C
C     BUFNUM      number of buffer allocated for table I/O; zero if
C                 table is not open
C     OFFSET      offset to I/O region in allocated buffer
C
C     IRET1       alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve buffer number for TABLE and set IRET to 0 or set IRET to
C     a non-zero value if TABLE is not a defined object:
C
      CALL OBINFO (TABLE, BUFNUM, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (BUFNUM .GT. 0) THEN
C
C           Perform the operation and set IRET accordingly:
C
            CALL TABIM (OPCODE, OBUFFR(OFFSET, BUFNUM), IMROW,
     *                  TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM), NUMPOL,
     *                  TIME, TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                  IFR, FREQVR, PDELAY, GDELAY, PRATE, GRATE,
     *                  DISP, DDISP, IRET)
            IF (IRET .GT. 0) THEN
               WRITE (MSGTXT, 7000) IRET
               CALL MSGWRT (7)
            END IF
C
C           The table buffer needs to be deallocated when the table is
C           closed.
C
            IF (OPCODE .EQ. 'CLOS') THEN
               CALL OBCLOS (TABLE, IRET1)
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 7001) IRET
                  CALL MSGWRT (7)
               END IF
            END IF
         ELSE
C
C           Table is not open.
C
            WRITE (MSGTXT, 7002)
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        TABLE does not name a recognized table object.
C
         WRITE (MSGTXT, 7003)
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OTABIM: TABLE I/O FAILED (ERROR ', I4, ')')
 7001 FORMAT ('OTABIM: FAILED TO DE-ALLOCATE BUFFER (ERROR ', I4, ')')
 7002 FORMAT ('OTABIM: TABLE IS NOT OPEN')
 7003 FORMAT ('OTABIM: TABLE OBJECT IS NOT DEFINED')
      END
      SUBROUTINE OIMINI (TABLE, OPCODE, IERR)
C-----------------------------------------------------------------------
C   Open interferoemeter table IMTAB for reading or writing (wraps
C   IMINI).  Note non-standard use of common-block for file I/O.
C
C   Inputs:
C      TABLE      C*(*)       Name of TABLE object referring to IM table
C      OPCODE     C*4         Mode ('READ' or 'WRIT')
C
C   Output:
C      IERR       I           Status
C                               zero indicates file opened
C                               anything else indicates a fatal error
C
C   Input/output from common(file keywords):
C      NOPOLZ         I    # polzns in the data
C      NPOLY          I    Order of polynomial used to form geometric
C                          model.
C      REVNUM         R    Revision # of correlator software used to
C                          generate correlator model.
C      OBSCODE        C*8  Observing code
C      NOSTKD         I    No polzns in the data
C      STK1           I    First Stokes parameter in the data
C      NOBAND         I    The number of bands (IF's) in the data.
C      NOCHAN         I    The number of spectral channels in the data.
C      REFFRQ         D    Freq. at reference pixel (Hz)
C      CHNBW          R    Bandwidth of single spectral channel (Hz)
C      REFPIX         R    Reference pixel
C      TABREV         I    Table revision number.
C                          Revision 1 - table invented, PJD, Feb 1, 1991
C      IMNUMV(MAXIMC) I    Element count in each column. On input only
C                          used if the file is created.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   IERR
C
      INTEGER   TDISK, TCNO, TVER, TROW, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'IM') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT IM'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL IMINI (OPCODE, OBUFFR(OFF, BUFNO), TDISK, TCNO, TVER,
     *   CATBLK, LUN, IERR)

      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, TROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OGTDEL (TABLE, CLTIME, CLSRC, CLSTA, CLARR, CLFQID,
     *   GEODLY, DISP, DDISP, NUMROW, IERR)
C-----------------------------------------------------------------------
C   Evaluate delay polynomials in the IM table for a given CL record.
C   The IM table should have been opened with OIMINI and should be
C   sorted in time order.
C   Inputs:
C      TABLE    C*?    Table object name
C      CLTIME   D      CL entry time (days)
C      CLSRC    I      CL entry source
C      CLSTA    I      CL entry antenna number
C      CLARR    I      CL entry array number
C      CLFQID   I      CL entry FQID
C   In/Out:
C      NUMROW   I      Set zero to force re-hash of (new) table
C                         else leave alone
C   Output:
C      GEODLY   D(*)   Delay polynomial
C      DISP     R(2)   Dispersive delay (sec)
C      DDISP    R(2)   Rate of change of DISP (sec/sec)
C      IERR     I      Error code, 0=>OK else TABNDX error.
C                         Note: -1=> record not found
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      INTEGER   CLSRC, CLSTA, CLARR, CLFQID, NUMROW, IERR
      REAL      DISP(2), DDISP(2)
      DOUBLE PRECISION CLTIME, GEODLY(*)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL GETDEL (CLTIME, CLSRC, CLSTA, CLARR, CLFQID,
     *   OBUFFR(OFF,BUFNO), GEODLY, DISP, DDISP, NUMROW, IERR)
      IF (IERR.LE.0) GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OMCINI (TABLE, OPCODE, MCROW, OBSCOD, RDATE, NSTOKE,
     *   STOKE1, NUMIF,  NCHAN, RFREQ, CHANBW, REFPIX, NUMPOL, FFTSIZ,
     *   OVRSMP, ZEROPD, TAPER, DELTAT, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes model components extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Input/output
C     OBSCOD       C*8  Observing code
C     RDATE        C*8  Reference date
C     NSTOKE       I    Number of "Stokes" channels in data
C     STOKE1       I    First "Stokes" channel number
C     NUMIF        I    Number of IFs
C     NCHAN        I    Number of channels
C     RFREQ        D    Reference frequency (Hz)
C     CHANBW       R    Channel bandwidth (Hz)
C     REFPIX       R    Reference pixel for frequency
C     NUMPOL       I    Number of polarizations in table
C     FFTSIZ       I    FFT size
C     OVRSMP       I    Oversampling factor
C     ZEROPD       I    Zero padding factor
C     TAPER        C*8  Tapering function ('HANNING' or 'UNIFORM')
C     DELTAT       R    Table time interval in days, default 2 min
C   Output:
C     CLROW        I   Next scan number, start of the file if 'READ',
C                       the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else TABINI or TABIO
C                      error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, OBSCOD*8, RDATE*8, TAPER*8
      INTEGER   MCROW, NSTOKE, STOKE1, NUMIF, NCHAN, NUMPOL, FFTSIZ,
     *          OVRSMP, ZEROPD, IERR
      DOUBLE PRECISION RFREQ
      REAL      CHANBW, REFPIX, DELTAT
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'MC') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT MC'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL MCINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, MCROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), OBSCOD, RDATE,
     *   NSTOKE, STOKE1, NUMIF, NCHAN, RFREQ, CHANBW, REFPIX, NUMPOL,
     *   FFTSIZ, OVRSMP, ZEROPD, TAPER, DELTAT, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, MCROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABMC (TABLE, OPCODE, MCROW, NUMPOL, NUMIF, TIME,
     *   SOURID, ANTNUM, SUBARR, FREQID, ATMOS, DATMOS, GDELAY, GRATE,
     *   CLOCK, DCLOCK, LOOFF, DLOOFF, DISP, DDISP, IRET)
C-----------------------------------------------------------------------
C   Read, write, or close a model components (MC) table.
C
C   If TABLE is an open MC table and OPCODE is 'READ' then set TIME
C   through DDISP from row MCROW of the table, increment MCROW by 1,
C   and set IRET to 0 if row MCROW is not flagged or a negative value if
C   row MCROW is flagged. If TABLE is an open MC table and OPCODE is
C   'READ' and row number MCROW can not be read then issue one or more
C   error messages and set IRET to a positive value.
C
C   If TABLE is an open MC table and OPCODE is 'WRIT' then write the
C   values of TIME through DDISP to row MCROW of the table, increment
C   MCROW by 1, and set IRET to 0. If TABLE is an open MC table and
C   OPCODE is 'WRIT' and row number MCROW can not be written then issue
C   one or more error messages and set IRET to a positive value.
C
C   If TABLE is an open MC table and OPCODE is 'CLOS' then close the
C   table and set IRET to 0. If TABLE is an open MC table and OPCODE is
C   'CLOS' and the table can not be closed then issue one or more error
C   messages and set IRET to a non-zero value.
C
C   If TABLE is not an open table then issue an error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)       name of table object
C      OPCODE  C*4         'READ', 'WRIT', or 'CLOS'
C      NUMPOL  I           number of polarizations in table (1 or 2)
C      NUMIF   I           number of IFs in table
C
C   Input/Output:
C      MCROW   I              row number to read or write; incremented
C                             on successful read or write
C      TIME    D              time in days - start of interval
C      SOURID  I              source ID number
C      ANTNUM  I              antenna number
C      SUBARR  I              subarray number
C      FREQID  I              frequency ID number
C      ATMOS   D              atmospheric delay in seconds
C      DATMOS  D              rate of change of atmospheric delay in
C                             seconds per second
C      GDELAY  D              group delay in seconds
C      GRATE   D              group rate in seconds per second
C      CLOCK   D(2)           clock offset in seconds per polarization
C      DCLOCK  D(2)           clock drift rate in seconds per second for
C                             each polarization
C      LOOFF   R(2, NUMIF)    LO offsets in Hz for each polarization and
C                             IF
C      DLOOFF  R(2, NUMIF)    rates of change of LO offsets in Hz per
C                             second for each polarization and IF
C      DISP    R(2)           dispersive delay in seconds at 1m for each
C                             polarization
C      DDISP   R(2)           rate of change of dispersive delay at 1m
C                             in seconds per second for each
C                             polarization
C
C   Output:
C      IRET    I           return status: negative if a flagged row was
C                                         read, 0 if operation complete,
C                                         non-zero if operation failed
C-----------------------------------------------------------------------
      CHARACTER        TABLE*(*)
      CHARACTER        OPCODE*4
      INTEGER          MCROW
      INTEGER          NUMPOL
      INTEGER          NUMIF
      DOUBLE PRECISION TIME
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      DOUBLE PRECISION ATMOS
      DOUBLE PRECISION DATMOS
      DOUBLE PRECISION GDELAY
      DOUBLE PRECISION GRATE
      DOUBLE PRECISION CLOCK(2)
      DOUBLE PRECISION DCLOCK(2)
      REAL             LOOFF(2, NUMIF)
      REAL             DLOOFF(2, NUMIF)
      REAL             DISP(2)
      REAL             DDISP(2)
      INTEGER          IRET
C
      INCLUDE 'TABSTUFF.INC'
C
C     Local Variables:
C
C     BUFNUM      number of buffer allocated for table I/O; zero if
C                 table is not open
C     OFFSET      offset to I/O region in allocated buffer
C
C     IRET1       alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve buffer number for TABLE and set IRET to 0 or set IRET to
C     a non-zero value if TABLE is not a defined object:
C
      CALL OBINFO (TABLE, BUFNUM, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (BUFNUM .GT. 0) THEN
C
C           Perform the operation and set IRET accordingly:
C
            CALL MCTAB (OPCODE, OBUFFR(OFFSET, BUFNUM), MCROW,
     *                  TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM), NUMPOL,
     *                  NUMIF, TIME, SOURID, ANTNUM, SUBARR, FREQID,
     *                  ATMOS, DATMOS, GDELAY, GRATE, CLOCK, DCLOCK,
     *                  LOOFF, DLOOFF, DISP, DDISP, IRET)
            IF (IRET .GT. 0) THEN
               WRITE (MSGTXT, 7000) IRET
               CALL MSGWRT (7)
            END IF
C
C           The table buffer needs to be deallocated when the table is
C           closed.
C
            IF (OPCODE .EQ. 'CLOS') THEN
               CALL OBCLOS (TABLE, IRET1)
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 7001) IRET
                  CALL MSGWRT (7)
               END IF
            END IF
         ELSE
C
C           Table is not open.
C
            WRITE (MSGTXT, 7002)
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        TABLE does not name a recognized table object.
C
         WRITE (MSGTXT, 7003)
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OTABMC: TABLE I/O FAILED (ERROR ', I4, ')')
 7001 FORMAT ('OTABMC: FAILED TO DE-ALLOCATE BUFFER (ERROR ', I4, ')')
 7002 FORMAT ('OTABMC: TABLE IS NOT OPEN')
 7003 FORMAT ('OTABMC: TABLE OBJECT IS NOT DEFINED')
      END
      SUBROUTINE OGETMC (TABLE, DELTAT, CLTIME, CLSRC, CLSTA, CLARR,
     *   CLFQID, CLOCK, DCLOCK, ATMOS, DATMOS, NUMROW, IERR)
C-----------------------------------------------------------------------
C   Extract atmospheric group-delay and clock offsets from an MC table
C   for a given CL record.  The MC table should hev been opened with
C   OMCINI and should be sorted in time order.
C   Inputs:
C      TABLE    C*?    Table object name
C      DELTAT   R      Time interval
C      CLTIME   D      CL entry time (days)
C      CLSRC    I      CL entry source
C      CLSTA    I      CL entry antenna number
C      CLARR    I      CL entry array number
C      CLFQID   I      CL entry FQID
C   In/out:
C      NUMROW   I      Set 0 to force a re-hash of MC table -
C                         otherwise leave alone
C   Output:
C      CLOCK    R(2)   Clock offsets (sec); 1 per polarization
C      DCLOCK   R(2)   Time-derivatives of clock offsets (sec/sec)
C      ATMOS    R      Atmospheric group delay (sec)
C      DATMOS   R      Time-derivative of atmospheric group delay
C                        (sec/sec)
C      IERR     I      Error code, 0=>OK else TABNDX error.
C                      Note: -1=> record not found
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      DOUBLE PRECISION CLTIME
      INTEGER   CLSRC, CLSTA, CLARR, CLFQID, NUMROW, IERR
      REAL      DELTAT, CLOCK(2), DCLOCK(2), ATMOS, DATMOS
C
      INTEGER   BUFNO, OFF
C
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL GETMC (OBUFFR(OFF,BUFNO), TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   DELTAT, CLTIME, CLSRC, CLSTA, CLARR, CLFQID,  CLOCK, DCLOCK,
     *   ATMOS, DATMOS, NUMROW, IERR)
      IF (IERR.LE.0) GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ONXINI (TABLE, OPCODE, NXROW, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes INDEX extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Output:
C     NXROW         I   Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C     IERR           I   Return error code, 0=>OK, else NDXINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   NXROW, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'NX') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT NX'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL NDXINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, NXROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, NXROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABNX (TABLE, OPCODE, NXROW, TIME, DTIME, IDSOUR,
     *   SUBARR, VSTART, VEND, FREQID, IERR)
C-----------------------------------------------------------------------
C   Does I/O to INDEX extension tables. Usually used after setup by
C   ONXINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     NXROW        I   Next scan number to read or write.
C   Input/output: (written to or read from INDEX file)
C     TIME         R   Center time of the scan (Days)
C     DTIME        R   Duration of scan (Days)
C     IDSOUR       I   Source ID as defined in then SOURCE table.
C     SUBARR       I   Subarray number.
C     VSTART       I   First visibility number in file.
C     VEND         I   Last visibility number in file.
C     FREQID       I   Freqid of scan
C   Output:
C     NXROW        I   Next scan number.
C     IERR         I   Error code, 0=>OK else TABNDX error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   NXROW, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR
      REAL      TIME, DTIME
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABNDX (OPCODE, OBUFFR(OFF,BUFNO), NXROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO),
     *   TIME, DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OOBINI (TABLE, OPCODE, OBROW, IERR)
C-----------------------------------------------------------------------
C   Open an orbit (OB) table for reading using OTABOB.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Output:
C     OBROW         I   Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C     IERR           I   Return error code, 0=>OK, else OBINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   OBROW, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'OB') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT OB'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL OBINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, OBROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, OBROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object so that it can be
C                                       reopened with TABOPN for use
C                                       with the generic TABLE routines
C                                       as well as OTABOB
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABOB (TABLE, OPCODE, OBROW, NOSTA, SUBARR, TIME,
     *                   POS, VEL, SUNANG, ECLIPS, ORIENT, IERR)
C-----------------------------------------------------------------------
C   Read/write records from/to an OB table or close an open OB table.
C   Inputs:
C     TABLE        C*?  Table object name
C     OPCODE       C*4  Operation code:
C                       'READ' = read entry from table.
C                       'WRIT' = write entry in table.
C                       'CLOS' = close file, flush on write
C     OBROW        I    Next scan number to read or write.
C   Input/output: (written to or read from orbit file)
C     NOSTA        I    Antenna number of orbiting antenna
C     SUBARR       I    Subarray to which the orbiting antenna belongs
C     TIME         D    Time to which record refers (days)
C     POS          D(3) Instantaneous position vector of antenna in
C                       the inertial reference frame defined by the
C                       equinox used in the data file (metres)
C     VEL          D(3) Instantaneous velocity vector in the same
C                       frame (metres per second)
C     SUNANG       R(3) Item 1: angle between the source and the sun
C                               (degrees)
C                       Items 2 and 3: reserved for future use
C     ECLIPS       R(4) Times since last eclipses (days)
C                       Item 1: time since last entering eclipse by
C                               the Earth
C                       Item 2: time since last leaving eclipse by
C                               the Earth
C                       Item 3: time since last entering lunar
C                               eclipse
C                       Item 4: time since last leaving lunar eclipse
C     ORIENT       R    Orientation of antenna with respect to celestial
C                       North (degrees); analagous to position angle for
C                       polarization calibration.
C   Output:
C     OBROW        I    Next row number.
C     IERR         I    Error code, 0=>OK else TABOB error.
C                       Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   OBROW, NOSTA, SUBARR, IERR
      REAL      SUNANG(3), ECLIPS(4), ORIENT
      DOUBLE PRECISION TIME, POS(3), VEL(3)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABOB (OPCODE, OBUFFR(OFF,BUFNO), OBROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), NOSTA, SUBARR, TIME, POS, VEL, SUNANG, ECLIPS,
     *   ORIENT, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OPCINI (TABLE, OPCODE, PCROW, NUMPOL, NUMIF, NUMTON,
     *                   IRET)
C-----------------------------------------------------------------------
C   Open phase cal (PC) table.
C
C   If TABLE references an existing PC table and OPCODE is 'READ' then
C   open the table for reading, set PCROW to 1, set NUMPOL, NUMIF, and
C   NUMTON from the table header, and set IRET to zero.
C
C   If TABLE references an existing PC table and OPCODE is 'WRIT' then
C   open the table for reading and writing, set PCROW to one more than
C   the number of rows in the table, set NUMPOL, NUMIF, and NUMTON from
C   the table header, and set IRET to zero.
C
C   If TABLE references an PC table that does not exist and OPCODE is
C   'WRIT' then create the PC table, open it for reading and writing,
C   fill the header from NUMPOL, NUMIF, and NUMTON, set PCROW to 1, and
C   set IRET to zero.
C
C   If TABLE does not reference a PC table, OPCODE is not 'READ' or
C   'WRIT', or if it is not possible to open the table then issue
C   one or more error messages and set IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)    name of table object
C      OPCODE  C*4      open mode: 'READ' or 'WRIT'
C
C   Input/Output:
C      NUMPOL  I        number of polarizations in table (1 or 2)
C      NUMIF   I        size of IF axis in parent data file
C      NUMTON  I        number of tones per IF
C
C   Output:
C      PCROW   I        next row to read or write
C      IRET    I        return status: 0 if table opened,
C                                      non-zero otherwise
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      CHARACTER OPCODE*4
      INTEGER   PCROW
      INTEGER   NUMPOL
      INTEGER   NUMIF
      INTEGER   NUMTON
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'TABSTUFF.INC'
C
C     Local variables:
C
C     BUFNUM   allocated buffer number
C     OFFSET   offset of I/O region in allocated buffer
C
C     TDISK    disk number for table
C     TCNO     catalogue number for table
C     TTYPE    table type
C     TVER     table version number
C
C     LUN      LUN allocated for table I/O
C
C     DIM      object attribute dimensions
C     CDUMMY   dummy character argument
C
C     IRET1    alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   TDISK
      INTEGER   TCNO
      CHARACTER TTYPE*2
      INTEGER   TVER
C
      INTEGER   LUN
C
      INTEGER   DIM(3)
      CHARACTER CDUMMY
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve table information and set IRET to 0 or set IRET to a non-
C     zero value if the table information can not be retrieved:
C
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (TTYPE .EQ. 'PC') THEN
C
C           Assign an I/O buffer for the table and set IRET to 0 or set
C           IRET to a non-zero value if no buffers are available:
C
            CALL OBOPEN (TABLE, IRET)
C
            IF (IRET .EQ. 0) THEN
C
C              Retrieve the buffer number and set IRET to 0:
C
               CALL OBINFO (TABLE, BUFNUM, IRET)
C
C              Assign a LUN for the table and set IRET to 0 or set IRET
C              to a non-zero value if no LUNs are available:
C
               CALL OBLUN (LUN, IRET)
C
               IF (IRET .EQ. 0) THEN
C
C                 Retrieve the catalogue header blcok for the parent
C                 file and set IRET to 0 or set IRET to a non-zero
C                 value if the catalogue header block can not be
C                 retrieved:
C
                  CALL OBHGET (TABLE, CATBLK, IRET)
C
                  IF (IRET .EQ. 0) THEN
C
C                    Open the table, update TVER to the actual version
C                    number of the table if it was zero before, and set
C                    IRET to 0 or set IRET to a non-zero value if it is
C                    not possible to open the table:
C
                     CALL PCINI (OPCODE, OBUFFR(OFFSET, BUFNUM), TDISK,
     *                           TCNO, TVER, CATBLK, LUN, PCROW,
     *                           TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM),
     *                           NUMPOL, NUMIF, NUMTON, IRET)
C
                     IF (IRET .EQ. 0) THEN
C
C                       Record the actual version number:
C
                        DIM(1) = 1
                        DIM(2) = 1
                        DIM(3) = 0
                        CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER,
     *                               CDUMMY, IRET)
C
C                       If a new table was created the catalogue header
C                       block is changed and needs to be written back to
C                       disk.
C
                        IF (OPCODE .EQ. 'WRIT') THEN
C
C                          Update the header block and set IRET to 0
C                          or set IRET to a non-zero value if it is not
C                          possible to update the catalogue header:
C
                           CALL OBHPUT (TABLE, CATBLK, IRET)
C
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 7000) IRET
                              CALL MSGWRT (7)
                           END IF
                        END IF
C
C                       The table needs to be closed and re-opened so
C                       that the generic table access routines in APLOOP
C                       will work.
C
                        CALL TABIO ('CLOS', 0, PCROW,
     *                              OBUFFR(OFFSET, BUFNUM),
     *                              OBUFFR(OFFSET, BUFNUM), IRET1)
                        IF (IRET1 .NE. 0) THEN
                           WRITE (MSGTXT, 7001) IRET1
                           CALL MSGWRT (7)
                           IRET = IRET1
                        END IF
                     ELSE
C
C                       Table could not be opened.
C
                        WRITE (MSGTXT, 7002) IRET
                        CALL MSGWRT (7)
                     END IF
                  ELSE
C
C                    Catalogue header could not be retrieved.
C
                     WRITE (MSGTXT, 7003) IRET
                     CALL MSGWRT (7)
                  END IF
C
C                 Free LUN:
C
                  CALL OBLUFR (LUN)
C
               ELSE
C
C                 Unable to allocate LUN.
C
                  WRITE (MSGTXT, 7004) IRET
                  CALL MSGWRT (7)
               END IF
C
C              Free the table buffer and set IRET1 to 0 or set IRET1 to
C              a non-zero value if it is not possible to free the
C              buffer:
C
               CALL OBCLOS (TABLE, IRET1)
C
               IF (IRET1 .NE. 0) THEN
                  WRITE (MSGTXT, 7005) IRET1
                  CALL MSGWRT (7)
                  IRET = IRET1
               END IF
            ELSE
C
C              Failed to allocate table buffer.
C
               WRITE (MSGTXT, 7006) IRET
               CALL MSGWRT (7)
            END IF
C
C           Re-open the table using the generic APLOOP table interface
C           and set IRET1 to 0 or set IRET1 to a non-zero value if the
C           table can not be re-opened:
C
            CALL TABOPN (TABLE, OPCODE, IRET1)
C
            IF (IRET1 .NE. 0) THEN
               WRITE (MSGTXT, 7007) IRET1
               CALL MSGWRT (7)
               IRET = IRET1
            END IF
         ELSE
C
C           TABLE does not refer to an PC table.
C
            WRITE (MSGTXT, 7008) TTYPE
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        Failed to retrieve table information.
C
         WRITE (MSGTXT, 7009) IRET
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OPCINI: FAILED TO UPDATE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7001 FORMAT ('OPCINI: FAILED TO CLOSE TABLE (ERROR ', I4, ')')
 7002 FORMAT ('OPCINI: FAILED TO OPEN TABLE (ERROR ', I4, ')')
 7003 FORMAT ('OPCINI: FAILED TO RETREIVE CATALOGUE HEADER (ERROR ', I4,
     *        ')')
 7004 FORMAT ('OPCINI: FAILED TO ALLOCATE LUN (ERROR ', I4, ')')
 7005 FORMAT ('OPCINI: FAILED TO FREE TABLE BUFFER (ERROR ', I4, ')')
 7006 FORMAT ('OPCINI: FAILED TO ALLOCATE TABLE BUFFER (ERROR ', I4,
     *        ')')
 7007 FORMAT ('OPCINI: FAILED TO RE-OPEN TABLE (ERROR ', I4, ')')
 7008 FORMAT ('OPCINI: TABLE MUST HAVE TYPE ''IM'' NOT ''', A2, '''')
 7009 FORMAT ('OPCINI: FAILED TO RETRIEVE TABLE INFORMATION (ERROR ',
     *        I4, ')')
      END
      SUBROUTINE OTABPC (TABLE, OPCODE, PCROW, NUMPOL, TIME, TIMINT,
     *                   SOURID, ANTNUM, SUBARR, FREQID, CABCAL, STATE,
     *                   PCFREQ, PCREAL, PCIMAG, PCRATE, IRET)
C-----------------------------------------------------------------------
C   Read, write, or close a phase cal (PC) table.
C
C   If TABLE is an open PC table and OPCODE is 'READ' then set TIME
C   through PCRATE from row PCROW of the table, increment PCROW by 1,
C   and set IRET to 0 if row PCROW is not flagged or a negative value if
C   row PCROW is flagged. If TABLE is an open PC table and OPCODE is
C   'READ' and row number PCROW can not be read then issue one or more
C   error messages and set IRET to a positive value.
C
C   If TABLE is an open PC table and OPCODE is 'WRIT' then write the
C   values of TIME through PCRATE to row PCROW of the table, increment
C   PCROW by 1, and set IRET to 0. If TABLE is an open PC table and
C   OPCODE is 'WRIT' and row number PCROW can not be written then issue
C   one or more error messages and set IRET to a positive value.
C
C   If TABLE is an open PC table and OPCODE is 'CLOS' then close the
C   table and set IRET to 0. If TABLE is an open PC table and OPCODE is
C   'CLOS' and the table can not be closed then issue one or more error
C   messages and set IRET to a non-zero value.
C
C   If TABLE is not an open table then issue an error message and set
C   IRET to a non-zero value.
C
C   Inputs:
C      TABLE   C*(*)       name of table object
C      OPCODE  C*4         'READ', 'WRIT', or 'CLOS'
C      NUMPOL   I           number of polarizations in table (1 or 2)
C
C   Input/Output:
C      PCROW   I               row number to read or write; incremented
C                              on successful read or write
C      TIME    D               time in days
C      TIMINT  R               time interval in days
C      SOURID  I               source ID number
C      ANTNUM  I               antenna number
C      SUBARR  I               subarray number
C      FREQID  I               frequency ID number
C      CABCAL  D               cable delay measurement in seconds
C      STATE   R(2, 4, *)      percentage of time spent in the
C                              lowest (1), medium-low (2),
C                              medium-high(3), and highest states by
C                              polarization and IF.
C      PCFREQ  D(2, MAXTON, *) phase tone frequencies by IF and
C                              polarization
C      PCREAL  R(2, MAXTON, *) real part of phase cal measurements by
C                              IF and polarization
C      PCIMAG  R(2, MAXTON, *) imaginary part of phase cal measurements
C                              by IF and polarization
C      PCRATE  R(2, MAXTON, *) rate of change of phase cal phase by
C                              IF and polarization
C
C   Output:
C      IRET    I           return status: negative if a flagged row was
C                                         read, 0 if operation complete,
C                                         non-zero if operation failed
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPCV.INC'
C
      CHARACTER        TABLE*(*)
      CHARACTER        OPCODE*4
      INTEGER          PCROW
      INTEGER          NUMPOL
      DOUBLE PRECISION TIME
      REAL             TIMINT
      INTEGER          SOURID
      INTEGER          ANTNUM
      INTEGER          SUBARR
      INTEGER          FREQID
      DOUBLE PRECISION CABCAL
      REAL             STATE(2, 4, *)
      DOUBLE PRECISION PCFREQ(2, MAXTON, *)
      REAL             PCREAL(2, MAXTON, *)
      REAL             PCIMAG(2, MAXTON, *)
      REAL             PCRATE(2, MAXTON, *)
      INTEGER          IRET
C
      INCLUDE 'TABSTUFF.INC'
C
C     Local Variables:
C
C     BUFNUM      number of buffer allocated for table I/O; zero if
C                 table is not open
C     OFFSET      offset to I/O region in allocated buffer
C
C     IRET1       alternate return status
C
      INTEGER   BUFNUM
      INTEGER   OFFSET
      PARAMETER (OFFSET = BUFSIZ - 511)
C
      INTEGER   IRET1
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Retrieve buffer number for TABLE and set IRET to 0 or set IRET to
C     a non-zero value if TABLE is not a defined object:
C
      CALL OBINFO (TABLE, BUFNUM, IRET)
C
      IF (IRET .EQ. 0) THEN
         IF (BUFNUM .GT. 0) THEN
C
C           Perform the operation and set IRET accordingly:
C
            CALL TABPC (OPCODE, OBUFFR(OFFSET, BUFNUM), PCROW,
     *                  TBKOLS(1, BUFNUM), TBNUMV(1, BUFNUM), NUMPOL,
     *                  TIME, TIMINT, SOURID, ANTNUM, SUBARR, FREQID,
     *                  CABCAL, STATE, PCFREQ, PCREAL, PCIMAG, PCRATE,
     *                  IRET)
            IF (IRET .GT. 0) THEN
               WRITE (MSGTXT, 7000) IRET
               CALL MSGWRT (7)
            END IF
C
C           The table buffer needs to be deallocated when the table is
C           closed.
C
            IF (OPCODE .EQ. 'CLOS') THEN
               CALL OBCLOS (TABLE, IRET1)
               IF (IRET1 .NE. 0) THEN
                  IRET = IRET1
                  WRITE (MSGTXT, 7001) IRET
                  CALL MSGWRT (7)
               END IF
            END IF
         ELSE
C
C           Table is not open.
C
            WRITE (MSGTXT, 7002)
            CALL MSGWRT (7)
            IRET = 1
         END IF
      ELSE
C
C        TABLE does not name a recognized table object.
C
         WRITE (MSGTXT, 7003)
         CALL MSGWRT (7)
      END IF
C-----------------------------------------------------------------------
 7000 FORMAT ('OTABPC: TABLE I/O FAILED (ERROR ', I4, ')')
 7001 FORMAT ('OTABPC: FAILED TO DE-ALLOCATE BUFFER (ERROR ', I4, ')')
 7002 FORMAT ('OTABPC: TABLE IS NOT OPEN')
 7003 FORMAT ('OTABPC: TABLE OBJECT IS NOT DEFINED')
      END
      SUBROUTINE OSNINI (TABLE, OPCODE, SNROW, NUMANT, NUMPOL, NUMIF,
     *   NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes solution (SN) extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Input/output
C     NUMANT       I    Number of antennas
C     NUMPOL       I    Number of IFs per group
C     NUMIF        I    Number of IF groups
C     NUMNOD       I    Number of interpolation nodes. Will handle
C                       up to 25 interpolation nodes.
C     GMMOD        R    Mean gain modulus
C     RANOD(*)     R    RA offset of interpolation nodes (deg.)
C     DECNOD(*)    R    Dec. offset of interpolation nodes (deg.)
C     ISAPPL       L    True if this SN table has been applied to
C                       the  CL table.
C   Output:
C     SNROW        I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else SNINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   SNROW, NUMANT, NUMPOL, NUMIF, NUMNOD, IERR
      LOGICAL   ISAPPL
      REAL       GMMOD, RANOD(25), DECNOD(25)
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'SN') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT SN'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL SNINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, SNROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, SNROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABSN (TABLE, OPCODE, SNROW, NUMPOL, TIME, TIMEI,
     *   SOURID, ANTNO, SUBA, FREQID, IFR, NODENO, MBDELY, DISP, DDISP,
     *   CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C-----------------------------------------------------------------------
C   Does I/O to solution (SN) extension tables. Usually used after
C   setup by OSNINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     SNROW        I   Next scan number to read or write.
C     NUMPOL       I   Number of polarizations per IF.
C   Input/output: (written to or read from solution file)
C     TIME         D   Center time of solution record (Days)
C     TIMEI        R   Time interval covered by record (days)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     ANTNO        I   Antenna number.
C     SUBA         I   Subarray number.
C     FREQID       I   Freqid #
C     IFR          R   Ionospheric Faraday Rotation (rad/m**2)
C     NODENO       I   Interpolation node number
C     MBDELY(2)    R   Multi band delays (sec) 1/Poln
C     MBDELY(2)    R   Multi band delays (sec) 1/Poln
C     DISP(2)      R   Dispersive delay (sec/m^2 scales as lambda^2)
C                      1/poln
C     DDISP(2)     R   Time derivative of DISP (sec/sec/m^2)
C     CREAL(2,*)   R   Real part of the complex gain, 1 /Poln/IF
C     CIMAG(2,*)   R   Imag part of the complex gain, 1 /Poln/IF
C     DELAY(2,*)   R   Residual group delay (sec), 1 /Poln/IF
C     RATE(2,*)    R   Residual fringe rate (Hz), 1 /Poln/IF
C     WEIGHT(2,*)  R   Weight of solution, 1 /Poln/IF
C     REFA(2,*)    R   Ref. ant. of solution, 1 /Poln/IF
C   Output:
C     SNROW       I   Next solution number.
C     IERR         I   Error code, 0=>OK else TABSN error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   SNROW, NUMPOL, SOURID, ANTNO, SUBA, FREQID, NODENO,
     *   REFA(2,*), IERR
      DOUBLE PRECISION TIME
      REAL      TIMEI, IFR, MBDELY(2), DISP(2), DDISP(2), CREAL(2,*),
     *   CIMAG(2,*), DELAY(2,*), RATE(2,*), WEIGHT(2,*)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABSN (OPCODE, OBUFFR(OFF,BUFNO), SNROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), NUMPOL, TIME, TIMEI, SOURID, ANTNO, SUBA,
     *   FREQID, IFR, NODENO, MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY,
     *   RATE, WEIGHT, REFA, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OSUINI (TABLE, OPCODE, NUMIF, VELTYP, VELDEF, FREQID,
     *   SUROW, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes SOURCE extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C   Input/Output:
C     NUMIF        I   Table keyword, gives the number of IFs
C     VELTYP       C*8 Velocity type,
C     VELDEF       C*8 Velocity defination 'RADIO','OPTICAL',
C     FREQID       I   Table keyword, denotes the FQ ID for which
C                      the SU parms have been modified. On O/P if
C                      FREQID = -999 it is not in the table, if
C                      FREQID = -1 the virgin values still exist, or
C                      the data have no FREQID random parameter.
C   Output:
C     SUROW        I   Next scan number, start of the file if 'READ',
C                      the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else SOUINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, VELTYP*8, VELDEF*8
      INTEGER   SUROW, NUMIF, FREQID, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'SU') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT SU'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL SOUINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, NUMIF, VELTYP, VELDEF, FREQID, SUROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, SUROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABSU (TABLE, OPCODE, SUROW, IDSOU, SOUNAM, QUAL,
     *   CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, IERR)
C-----------------------------------------------------------------------
C   Does I/O to SOURCE extension tables. Usually used after setup by
C   OSUINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     SUROW       I   Next scan number to read or write.
C   Input/output: (written to or read from SOURCE file)
C     IDSOUR       I   Source ID as defined in the SOURCE table.
C     SOUNAM       C*16 Source name
C     QUAL         I   Source qualifier.
C     CALCOD       C*4 Calibrator code
C     FLUX(4,?)    R   Total flux density I, Q, U, V pol, (Jy)
C                      1 set per IF.
C     FREQO(?)     D   Frequency offset (Hz) from IF nominal.
C     BANDW        D   Bandwidth (Hz)
C     RAEPO        D   Right ascension at mean EPOCH (degrees)
C     DECEPO       D   Declination at mean EPOCH (degrees)
C     EPOCH        D   Mean Epoch for position in yr. since year 0.0
C     RAAPP        D   Apparent Right ascension (degrees)
C     DECAPP       D   Apparent Declination(degrees)
C     RAOBS        D   Pointing Right ascension (degrees)
C     DECOBS       D   Pointing Declination(degrees)
C     LSRVEL(?)    D   LSR velocity (m/sec) of each IF
C     LRESTF(?)    D   Line rest frequency (Hz) of each IF
C     PMRA         D   Proper motion (deg/day) in RA
C     PMDEC        D   Proper motion (deg/day) in declination
C   Output:
C     SUROW        I   Next source number.
C     IERR         I   Error code, 0=>OK else TABSOU error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, CALCOD*4, SOUNAM*16
      INTEGER   SUROW, IDSOU, QUAL, IERR
      REAL      FLUX(4,*)
      DOUBLE PRECISION    FREQO(*), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL(*), LRESTF(*), PMRA, PMDEC
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABSOU (OPCODE, OBUFFR(OFF,BUFNO), SUROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO,
     *   BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *   LSRVEL, LRESTF, PMRA, PMDEC, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTYINI (TABLE, OPCODE, TYROW, NUMPOL, NUMIF, IERR)
C-----------------------------------------------------------------------
C   Creates and initializes T sys (TY) extension tables.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'WRIT' = create/init for write or read
C                      'READ' = open for read only
C     NUMPOL       I   Number of polarizations (1 or 2)
C     NUMIF        I   Number of IFs for which data is stored in file
C   Output:
C     TYROW        I   Next row number, 1 if READ, the last+1 if WRITE
C     IERR         I   Return error code, 0=>OK, else TYINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   TYROW, NUMPOL, NUMIF, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'TY') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT TY'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL TYINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, TYROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   NUMPOL, NUMIF, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, TYROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABTY (TABLE, OPCODE, TYROW, NUMPOL, NUMIF, TIME,
     *   TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT, IERR)
C-----------------------------------------------------------------------
C   Does I/O to TSYS (TY) extension tables. Usually used after
C   setup by OTYINI.
C   Inputs:
C     TABLE        C*? Table object name
C     OPCODE       C*4 Operation code:
C                      'READ' = read entry from table.
C                      'WRIT' = write entry in table.
C                      'CLOS' = close file, flush on write
C     TYROW        I   Next record number to read or write.
C     NUMPOL       I   Number of polarizations (1 or 2)
C     NUMIF        I   Number of IFs in TY file
C   Input/output: (written to or read from Tsys file)
C     TIME         R   Time of record (Days)
C     TIMEI        R   Time interval covered by record (days)
C     SOURID       I   Source ID as defined in the SOURCE table.
C     ANTNO        I   Antenna number.
C     SUBA         I   Subarray number.
C     FREQID       I   Freqid #
C     TSYS(2,*)    R   System temperature in Kelvin (one per poln/IF)
C     TANT(2,*)    R   Antenna temperature in Kelvin (one per poln/IF)
C    Output:
C     TYROW       I   Next record number.
C     IERR         I   Error code, 0=>OK else TABTY error.
C                      Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   TYROW, NUMPOL, NUMIF, SOURID, ANTNO, SUBA, FREQID, IERR
      REAL      TIME, TIMEI, TSYS(2,NUMIF), TANT(2,NUMIF)
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABTY (OPCODE, OBUFFR(OFF,BUFNO), TYROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO,
     *   SUBA, FREQID, TSYS, TANT, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OWXINI (TABLE, OPCODE, WXROW, OBSCOD, OBSDAT, TABVER,
     *   IERR)
C-----------------------------------------------------------------------
C   Creates and initializes weather (WX) extension tables.
C   Inputs:
C      TABLE    C*?   Table object name
C      OPCODE   C*4   Operation code:
C                        'WRIT' = create/init for write or read
C                        'READ' = open for read only
C      OBSCOD   C*8   Observing code.
C      OBSDAT   C*8   Observing date.
C      TABVER   I     Table revision number.
C   Output:
C      WXROW    I     Next row number, 1 if READ, the last+1 if WRITE
C      IERR     I     Return error code, 0=>OK, else WXINI error.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4, OBSCOD*8, OBSDAT*8
      INTEGER   WXROW, TABVER, IERR
C
      INTEGER   TDISK, TCNO, TVER, BUFNO, OFF, LUN, DIM(3)
      CHARACTER TTYPE*2, CDUMMY*1
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Check extension type
      IF (TTYPE.NE.'WX') THEN
         MSGTXT = 'INCORRECT TABLE TYPE: ' // TTYPE // ' NOT WX'
         CALL MSGWRT(7)
         IERR = 7
         GO TO 900
         END IF
C                                       Open object (assign buffer)
      CALL OBOPEN (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Create/initialize file
      CALL WXINI (OPCODE, OBUFFR(OFF,BUFNO), TDISK, TCNO, TVER, CATBLK,
     *   LUN, WXROW, TBKOLS(1,BUFNO), TBNUMV(1,BUFNO),
     *   OBSCOD, OBSDAT, TABVER, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Save CATBLK on write
      IF (OPCODE.EQ.'WRIT') CALL OBHPUT (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Save version number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TVER, CDUMMY, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close table
      CALL TABIO ('CLOS', 0, WXROW, OBUFFR(OFF,BUFNO),
     *   OBUFFR(OFF,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close object (deassign buffer)
      CALL OBCLOS (TABLE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Open the AIPS object
      CALL TABOPN (TABLE, OPCODE, IERR)
      IF (IERR.GT.0) GO TO 900
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTABWX (TABLE, OPCODE, WXROW, TIME, DTIME, IANT, SUBA,
     *   TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP, H2OCOL, IONCOL,
     *   IERR)
C-----------------------------------------------------------------------
C   Does I/O to weather (WX) extension tables. Usually used after
C   setup by OWXINI.
C   Inputs:
C      TABLE    C*?   Table object name
C      OPCODE   C*4   Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                         'CLOS' = close file, flush on write
C      WXROW    I     Next record number to read or write.
C   Input/output: (written to or read from Tsys file)
C      TIME     D     Time (days wrt reference day)
C      DTIME    R     integration time (days)
C      IANT     I     antenna number
C      SUBA     I     subarray number
C      TEMP     R     surface temperature (C)
C      PRESS    R     surface pressure (mbar)
C      DEWPT    R     dew point temperature (C)
C      WVEL     R     wind velocity (m/s)
C      WDIR     R     wind direction (east from north)
C      WGUST    R     wind gusts (m/s)
C      PRECIP   R     precipitation (cm)
C      H2OCOL   R     water column (m^-2)
C      IONCOL   R     ion column (m^-2)
C    Output:
C      WXROW    I     Next record number.
C      IERR     I     Error code, 0=>OK else TABWX error.
C                         Note: -1=> read but record deselected.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*), OPCODE*4
      INTEGER   WXROW, IANT, SUBA, IERR
      DOUBLE PRECISION TIME
      REAL      DTIME, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL
C
      INTEGER   BUFNO, OFF
      INCLUDE 'TABSTUFF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OFF = BUFSIZ - 511
C                                       Get buffer number
      CALL OBINFO (TABLE, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Table open?
      IF (BUFNO.LE.0) THEN
         MSGTXT = TABLE // ' TABLE IS NOT OPEN'
         CALL MSGWRT (6)
         IERR = 5
         GO TO 999
         END IF
C                                        Transfer
      CALL TABWX (OPCODE, OBUFFR(OFF,BUFNO), WXROW, TBKOLS(1,BUFNO),
     *   TBNUMV(1,BUFNO), TIME, DTIME, IANT, SUBA, TEMP, PRESS, DEWPT,
     *   WVEL, WDIR, WGUST, PRECIP, H2OCOL, IONCOL, IERR)
      IF (IERR.GT.0) GO TO 900
C                                       Close?
      IF (OPCODE.EQ.'CLOS') THEN
         CALL OBCLOS (TABLE, IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH TABLE OBJECT ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OSUELV (TABLE, ANT, SUBARR, SOURID, TIME, HA, ELEV, AZ,
     *   IERR)
C-----------------------------------------------------------------------
C   Returns the elevation of a source at a given antenna and time.
C   Inputs:
C      INTAB   C*?  Input table object (assumed to have associated AN
C                   and SU tables)
C      ANT     I    Antenna number
C      SUBARR  I    Subarray number 0=> 1
C      SOURID  I    Source ID number, use -1 for single source data
C      TIME    D    Time in days
C   Output:
C      HA      R    Source hour angle in radians
C      ELEV    R    Source elevations in radians
C      AZ      R    Source azimuth in radians
C      IERR    I    Return code, O=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      INTEGER   ANT, SOURID, SUBARR, IERR
      DOUBLE PRECISION TIME
      REAL      HA, ELEV, AZ
C
      INTEGER   TDISK, TCNO, TVER, LUN
      CHARACTER TTYPE*2
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set antennas
      CALL GETANT (TDISK, TCNO, SUBARR, CATBLK, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set source if necessary
      IF ((SOURID.NE.IDSOUR) .OR. (SOURID.LE.0)) CALL GETSOU (SOURID,
     *   TDISK, TCNO, CATBLK, LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Determine value
      CALL SOUELV (ANT, TIME, HA, ELEV, AZ)
C                                       Free LUN
      CALL OBLUFR (LUN)
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM SOURCE ELEVATIONS FOR ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OSUPAN (TABLE, ANT, SUBARR, SOURID, TIME, PANGL, IERR)
C-----------------------------------------------------------------------
C   Returns the elevation of a source at a given antenna and time.
C   Inputs:
C      INTAB   C*?  Input table object (assumed to have associated AN
C                   and SU tables)
C      ANT     I    Antenna number
C      SUBARR  I    Subarray number 0=> 1
C      SOURID  I    Source ID number, use -1 for single source.
C      TIME    R    Time in days
C   Output:
C      PANGL   R    Source parallactic angle in radians
C      IERR    I    Return code, O=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      INTEGER    ANT, SOURID, SUBARR, IERR
      REAL       TIME, PANGL
C
      INTEGER   TDISK, TCNO, TVER, LUN
      CHARACTER TTYPE*2
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      PANGS(MAXANT)
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set antennas
      CALL GETANT (TDISK, TCNO, SUBARR, CATBLK, SBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set source if necessary
      IF ((SOURID.NE.IDSOUR) .OR. (SOURID.LE.0)) CALL GETSOU (SOURID,
     *   TDISK, TCNO, CATBLK, LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Determine value
      CALL PARANG (TIME, PANGS)
      PANGL = PANGS(ANT)
C                                       Free LUN
      CALL OBLUFR (LUN)
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM WITH PARALLACTIC ANGLE FOR ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
