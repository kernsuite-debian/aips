LOCAL INCLUDE 'GSCAT.INC'
C                                       Local include for GSCAT
      INTEGER   IBLNK
      INTEGER   CATBLK(256), FDVEC(50), TBIND, NTAPE, CNO, NBPIX,
     *   TAPEBP, TABLES, INBUFF(4096), TAPBUF(29184), UNKNWN
      LOGICAL   ISBLNK, FUCKUP, STDEXT, DODISK
      REAL      CATR(256), SHIFT(2)
      HOLLERITH CATH(256), XNAME(3), XCLASS(2), XNAME0(3), XCLAS0(2),
     *   XINFIL(12), XJNFIL(12)
      CHARACTER TNAME*48, NAME*12, CLASS*6, INFILE*48, HDRBUF*2880
      DOUBLE PRECISION CATD(128), POS11(2), SCALE, OFFSET, ISCALE,
     *   IZERO
      REAL      NTAPE4, SEQ4, KVOL4, NCOUNS, DOTABL, NFILES, NMAPS
      REAL      NTAPE0, SEQ0, KVOL0, NCOUN0, DOTAB0, NFILE0, NMAPS0
      COMMON /MLTAP/ SCALE, OFFSET, ISCALE, IZERO, POS11, TAPBUF,
     *   INBUFF, IBLNK, ISBLNK, FUCKUP, STDEXT, DODISK, UNKNWN,
     *   FDVEC, TBIND, NTAPE, CNO, NBPIX, TAPEBP, TABLES
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XINFIL, SHIFT, NTAPE4, XNAME, XCLASS, SEQ4,
     *   KVOL4, NCOUNS, DOTABL, NFILES, NMAPS
      COMMON /DUMPAR/ NTAPE0, XNAME0, XCLAS0, SEQ0, KVOL0, NCOUN0,
     *   DOTAB0, NFILE0, NMAPS0, XJNFIL
      COMMON /CHRCOM/ HDRBUF, NAME, CLASS, INFILE, TNAME
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C                                                          End GSCAT
LOCAL END
      PROGRAM GSCAT
C-----------------------------------------------------------------------
C! Finds the Guide Star Cataloge region containing the input RA+DEC
C# Map-util EXT-util FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2004, 2010, 2015
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
C-----------------------------------------------------------------------
C   GSCAT reads in the REGIONS Table for the Guides Star cataloge and
C   returns the field containing the users input RA and Dec.
C   This program will be executed assuming the REGIONS table
C   in a DISK FITS file.
C   AIPS input parameters:
C      INFILE     R(12) Name of file containing the Guide star FITSC
C                       Table of Field boundaries.
C      SHIFT      R(2)  Ra (hours, J2000) and Dec (degrees) of the
C                       Field for the requested reqion number
C-----------------------------------------------------------------------
      INTEGER   IRET, SEQ, KVOL, HLUN, HBUFF(256), FITS, ISCR(256),
     *   I, N, IERR, NPARM, NMPSKP, IROUND, LFITS
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA HLUN, NPARM /27, 14/
C-----------------------------------------------------------------------
      LFITS = 0
      CALL MLINI (NPARM, KVOL, SEQ, ISCR, IRET)
      IF (IRET.NE.0) GO TO 995
      UNKNWN = 0
C                                       Convert to internal format.
      N = NCOUNS + .05
      IF (N.LE.0) N = 1
C                                       Open and position tape.
      NMPSKP = IROUND (NFILES)
      CALL MLTAPE (NMPSKP, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       IBM format positioning
      NMPSKP = IROUND (NMAPS)
      CALL RCOPY (NPARM, NTAPE4, NTAPE0)
C                                       Loop over requested images
      DO 100 I = 1,N
C                                       Virgin copy of parms
 20      CALL RCOPY (NPARM, NTAPE0, NTAPE4)
C                                       See what kind of file.
         IF (IRET.NE.0) LFITS = 0
         CALL TPIOHD (FDVEC, 128, FITS, TBIND, TAPBUF, ISCR, IRET)
         IF ((LFITS.EQ.2) .AND. (IRET.EQ.4)) GO TO 20
         IF (IRET.NE.0) GO TO 950
         IF ((FITS.EQ.1) .OR. (FITS.EQ.2)) GO TO 50
            WRITE (MSGTXT,1000)
            IF (FITS.EQ.-1) WRITE (MSGTXT,1001)
            IF ((FITS.EQ.-1) .AND. (FDVEC(42).EQ.2560))
     *         WRITE (MSGTXT,1002)
            CALL MSGWRT (8)
            IRET = 1
            GO TO 950
C                                       The tape is recognizable
 50      LFITS = FITS
         IF (FITS.EQ.1) CALL FITTAP (HLUN, HBUFF, IRET)
         IF (IRET.NE.0) GO TO 950
 100     CONTINUE
C                                       Close output
 950  CONTINUE
      CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IRET.EQ.0) IRET = IERR
C                                       Clean up
 995  CALL DIE (IRET, HBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPE IS UV-EXPORT FORMAT: USE UVLOD')
 1001 FORMAT ('TAPE FILE IS NON-STANDARD FITS WHICH IS NOT SUPPORTED')
 1002 FORMAT ('TAPE IS LIKELY TO BE ''RPFITS'' FORMAT: TRY ATLOD')
      END
      SUBROUTINE MLINI (NPARM, KVOL, SEQ, ISCR, IRET)
C-----------------------------------------------------------------------
C   MLINI initializes GSCAT
C   Outputs:
C      KVOL   I     Output disk #
C      SEQ    I     Output sequence number
C      ISCR   I(256)   Scratch buffer.
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   KVOL, ISCR(256), IRET, SEQ, I1
      INTEGER   NPARM, IERR,IROUND
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'GSCAT '/
C-----------------------------------------------------------------------
C                                       Initialize disk character.
      I1 = 1
      CALL ZDCHIN (.TRUE., ISCR)
      CALL HIINIT (3)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Initialize for AIPS
      IRET = 0
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XINFIL, ISCR, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR
         RQUICK = .FALSE.
         GO TO 990
C                                       Check if disk file output
 20   CALL H2CHR (48, 1, XINFIL, INFILE)
C                                       Set default if blank
      IF (INFILE.EQ.' ') INFILE = 'APLCONTR:REGIONS.TBL'
      DODISK = INFILE.NE.' '

      IF (((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) .OR. (DODISK))
     *    GO TO 30
         WRITE (MSGTXT,1020)
         RQUICK = .FALSE.
         GO TO 990
 30   IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
C                                       Fill in parameters and defaults
      SEQ = IROUND (SEQ4)
      NTAPE = IROUND (NTAPE4)
      IF (NTAPE.EQ.0) NTAPE = 1
      KVOL = IROUND (KVOL4)
C                                       Init FDVEC
      CALL FILL (50, 0, FDVEC)
      GO TO 999
C
 990  IRET = 16
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR IN GTPARM.  IER=',I7)
 1020 FORMAT ('TAPES USED ONLY WITH INTERACTIVE AIPS')
      END
      SUBROUTINE SNEVAL (NPIX, SNDIV, SNCUT, SUBVAL, INB)
C----------------------------------------------------------------------
C   SNEVAL corrects a buffer for the presence of blanking substituting
C   a specified value for the blanked pixels.  Unblanked drops through
C   quickly as well.
C   Inputs : NPIX    I*2        Number of pixels in buffers
C            SNDIV   I*2        Header blanking value S2H(K2INH)
C            SNCUT   I*2        S/N >= SNCUT acceptable: n-bit blanking
C            SUBVAL  I*2        Value to give blanked pixels
C   In/out : INB     I*2(NPIX)  Input buffer & output corrected buffer
C----------------------------------------------------------------------
      INTEGER   NPIX, SNDIV, SNCUT, SUBVAL, INB(1)
      INTEGER   I, ITEMP, ISN, DIV, SNC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
      IF (NPIX.LE.0) GO TO 999
      IF (SNDIV.EQ.0) GO TO 999
      IF (SNDIV.EQ.BLANKV) GO TO 100
      IF ((SNDIV.GT.0) .AND. (SNDIV.LE.8)) GO TO 10
         WRITE (MSGTXT,1000) SNDIV
         CALL MSGWRT (6)
C                                       Blank with multiple bits
 10   CONTINUE
         DIV = 2 ** SNDIV
         SNC = MIN0 (SNCUT, DIV-1)
         DO 20 I = 1,NPIX
            ITEMP = INB(I)
            ISN = MOD (IABS(ITEMP), DIV)
            IF (ISN.GE.SNC) INB(I) = ITEMP/DIV
            IF (ISN.LT.SNC) INB(I) = SUBVAL
 20         CONTINUE
         GO TO 999
C                                       Blank with magic value
 100  IF (SUBVAL.EQ.BLANKV) GO TO 999
         DO 120 I = 1,NPIX
            IF (INB(I).EQ.BLANKV) INB(I) = SUBVAL
 120        CONTINUE
C
 999  RETURN
C---------------------------------------------------------------------
 1000 FORMAT (' ILLEGAL BLANKING VALUE =',I8)
      END
      SUBROUTINE FITTAP (HLUN, HBUFF, IERR)
C-----------------------------------------------------------------------
C  Process FITS type tape header, data, and extension files.
C  Inputs:
C     HLUN   I        Open History file LUN.
C     HBUFF  I(256)   History file work I/O buffer.
C  Outputs:
C     IERR   I        Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   HLUN, HBUFF(256), IERR
      INTEGER   ISLOT, SEQ, KVOL, NUMTAB, IROUND, IOP, ITSAVE
      LOGICAL   NODATA, EOF, MORTAB
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      KVOL = KVOL4 + .5
      SEQ = IROUND (SEQ4)
      IOP = 1
C                                       Does header & history using a
C                                       temporary name in catalog.
 10   CALL FITHDR (IOP, KVOL, ISLOT, NODATA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get the data and store in file
      IF (.NOT.NODATA) CALL FITDAT (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Add inputs file to history.
      ITSAVE = CATBLK(KIIMS)
      CATBLK(KIIMS) = SEQ
      CATBLK(KIIMS) = ITSAVE
C                                       Standard fits extension records.
      CALL FITRXM (HLUN, HBUFF, NUMTAB, EOF, IERR)
      MORTAB = IERR.LT.0
      IERR = 0
C                                       Old tables records.
      IF (.NOT.EOF) CALL MLTABL (KVOL, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (UNKNWN.GT.0) THEN
         WRITE (MSGTXT,1030) UNKNWN
         CALL MSGWRT (6)
         END IF
C                                       If no data use table name for
C                                       image.
C         IF (NODATA) CALL FIXNDT (NUMTAB, IERR)
C                                       Fill in default names if needed.
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL MLDEF (NAME, CLASS, SEQ)
C                                       Trap case where there were too
C                                       many tables of a given type and
C                                       a dummy file is needed for the
C                                       excess tables.
      IF (MORTAB) THEN
         EOF = .FALSE.
         NODATA = .TRUE.
         IOP = 2
         IF (SEQ.GT.0) SEQ = SEQ + 1
         WRITE (MSGTXT,1050)
         CALL MSGWRT (6)
         GO TO 10
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('WARNING:',I7,' unknown records skipped while reading',
     *   ' tape')
 1050 FORMAT ('Too many tables for a single file, must create another')
      END
      SUBROUTINE FITDAT (IER)
C-----------------------------------------------------------------------
C   FITDAT reads the input data file and scales the data to disk.
C   Outputs:
C      IER   I     Error return:  0--> okay
C                                 1--> error condition
C-----------------------------------------------------------------------
      INTEGER   IER
C
      CHARACTER CDECLS(5)*4, CHTM12*12
      INTEGER   BLKS, IERR, IWIN(4), NBKOF1, IOFF, NX, NY,
     *   IDEPTH(5), NBYB, I, INX, INY, IBL, ITEMP,
     *   NXY, I3, I3B, I4, I4B, I5, I5B, I6, I6B, I7, I7B, DLUN,
     *   NTAPVL, J, NDECLS, III, L0, L1, L2, NXX
      REAL      BUFF(4096), MMAX, MMIN, INBUFR(1)
      DOUBLE PRECISION    BSC, BZE
      LOGICAL   T, BACK, WASBLK
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUFR(1), INBUFF(1))
      DATA T /.TRUE./
      DATA NDECLS, CDECLS /5, 'DEC ', 'DEC-', 'MM  ', 'GLAT', 'ELAT'/
C-----------------------------------------------------------------------
      DLUN = 16
      MMAX = -1.E20
      MMIN =  -MMAX
      WASBLK = .FALSE.
C                                       Initialize
      IER = 0
      BSC = SCALE
      IF (BSC.EQ.0.0D0) BSC = 1.0D0
      BZE = OFFSET
      NBYB = 4096 * 2
C                                       second axis backwards?
      BACK = .FALSE.
      IF (CATR(KRCIC+1).LT.0.0) THEN
         J = KHCTP + 2
         DO 6 I = 1,NDECLS
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            BACK = CHTM12(1:4) .EQ. CDECLS(I)(1:4)
            IF (BACK) GO TO 8
  6         CONTINUE
         GO TO 10
  8      CONTINUE
            CATR(KRCIC+1) = -CATR(KRCIC+1)
            CATR(KRCRP+1) = CATBLK(KINAX+1) + 1 - CATR(KRCRP+1)
         END IF
C                                       Set window parms
 10   I3B = MAX (1, CATBLK(KINAX+2))
      I4B = MAX (1, CATBLK(KINAX+3))
      I5B = MAX (1, CATBLK(KINAX+4))
      I6B = MAX (1, CATBLK(KINAX+5))
      I7B = MAX (1, CATBLK(KINAX+6))
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATBLK(KINAX)
      IWIN(4) = CATBLK(KINAX+1)
      NY = IWIN(4)
      NX = IWIN(3)
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
      IF (BACK) THEN
         IWIN(2) = NY
         IWIN(4) = 1
         END IF
C                                       Initialize tape
      NTAPVL = 1440
      NBPIX = TAPEBP
      IF (NBPIX.EQ.8) NTAPVL = 2880
      IF (ABS(NBPIX).EQ.32) NTAPVL = 720
      IOFF = NTAPVL
      BLKS = (ABS(NBPIX) / 8)
      BLKS = BLKS * NX * NY * I3B
      BLKS = BLKS * I4B * I5B * I6B * I7B
      BLKS = (BLKS - 1) / 2880 + 1
      BLKS = BLKS - 1
      IF (IERR.NE.0) GO TO 970
C                                       Test for Kitt Peak "error"
      IF ((IBLNK.EQ.0) .AND. (NBPIX.EQ.8)) ISBLNK = .FALSE.
      DO 200 I7 = 1,I7B
      DO 200 I6 = 1,I6B
      DO 200 I5 = 1,I5B
      DO 200 I4 = 1,I4B
      DO 200 I3 = 1,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IERR
            GO TO 980
C                                       Begin read/write loop
 30      DO 195 I = 1,NY
            NXY = NX
            IBL = 0
C                                       Copy and read until entire map
C                                       row filled.
 55         NXX = MIN (NXY, NTAPVL-IOFF)
C                                       Need more tape values.
               IF (NXX.GT.0) GO TO 60
                  BLKS = BLKS - 1
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  IOFF = 0
                  IF (NBPIX.EQ.8) CALL ZI8IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.16) CALL ZI16IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.32) CALL ZI32IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-32) CALL ZR32RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (IERR.EQ.0) GO TO 55
                     GO TO 970
C                                       INT in: copy convert max/min
 60            IF ((NBPIX.NE.8) .AND. (NBPIX.NE.16) .AND.
     *            (NBPIX.NE.32)) GO TO 150
                  L0 = IOFF
                  L2 = IBL
                  IF (ISBLNK) THEN
                     DO 100 III = 1,NXX
                        L1 = L2 + III
                        ITEMP = INBUFF(L0+III)
C                                       Blank pixel found
                        IF (ITEMP.EQ.IBLNK) THEN
                           BUFF(L1) = FBLANK
                           WASBLK = .TRUE.
C                                       scale
                        ELSE
                           BUFF(L1) = BSC * ITEMP + BZE
                           MMIN = MIN (MMIN, BUFF(L1))
                           MMAX = MAX (MMAX, BUFF(L1))
                           END IF
 100                    CONTINUE
                  ELSE
                     DO 115 III = 1,NXX
                        L1 = L2 + III
                        BUFF(L1) = BSC * INBUFF(L0+III) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
 115                    CONTINUE
                     END IF
                  GO TO 190
C                                       IEEE in
 150           CONTINUE
                  L0 = IOFF
                  L2 = IBL
                  DO 160 III = 1,NXX
                     L1 = L2 + III
                     BUFF(L1) = INBUFR(L0+III)
                     IF (BUFF(L1).NE.FBLANK) GO TO 155
                        WASBLK = .TRUE.
                        GO TO 160
 155                 CONTINUE
                        BUFF(L1) = BSC * BUFF(L1) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
 160                 CONTINUE
C                                       Up the counters
 190           IBL = IBL + NXX
               IOFF = IOFF + NXX
               NXY = NXY - NXX
C                                       loop back if needed to finish
               IF (NXY.GT.0) GO TO 55
 195         CONTINUE
C                                       Flush this plane.
 200     CONTINUE
C                                       close files
      CATR(KRDMX) = MMAX
      CATR(KRDMN) = MMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
C      CALL MAPCLS ('WRIT', KVOL, CNO, DLUN, DIND, CATBLK, T, BUFF, IERR)
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 970  WRITE (MSGTXT,1970) IERR
 980  CALL MSGWRT (8)
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FITDAT: COULD NOT INITIALIZE DISK FILE.  IER=',I4)
 1970 FORMAT ('FITDAT: COULD NOT READ INPUT.  IER=',I4)
      END
      SUBROUTINE FIXNDT (NUMTAB, IERR)
C-----------------------------------------------------------------------
C  This routine will fix up the map header of a tape with no data
C  section, but with at least one extension table.
C  Inputs:
C     NUMTAB  I    Number of tables files.  0 produces an error message.
C  Output:
C     IERR    I    Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   NUMTAB, IERR
C
      CHARACTER CHTM12*12, CNAME*12, CCLASS*6, STAT*4
      REAL      BUFF(128)
      INTEGER   LBFSZ, DLUN, INX, INY, IWIN(4), NBKOF
      LOGICAL   T
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DLUN, INX, INY, IWIN, NBKOF /16,1,1, 4*1, 1/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IF (NUMTAB.LE.0) GO TO 980
C                                       Fix up header.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTM12)
      IF (CHTM12.EQ.' ') CALL CHR2H (12, EXTNAM, KHIMNO, CATH(KHIMN))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTM12)
      IF (CHTM12(1:6).EQ.' ') CALL CHR2H (6, EXTTYP, KHIMCO,
     *   CATH(KHIMC))
C                                       Coordinates.
      CALL CHR2H (8, 'DUMMY   ', 1, CATH(KHCTP))
      CALL CHR2H (8, 'DUMMY   ', 1, CATH(KHCTP+2))
C                                       Max, min.
      CATR(KRDMX) = 1.0
      CATR(KRDMN) = 0.0
      CATR(KRBLK) = FBLANK
C                                       Open map file.
      LBFSZ = 128 * 2
C                                       Write first row
      BUFF(1) = 1.0
      BUFF(2) = 0.0
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CCLASS)
      STAT = 'CLWR'
C                                       Clear in /CFILES/
      NCFILE = NCFILE - 1
      GO TO 999
C                                       No tables.
 980  IERR = 1
      WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('NO DATA AND NO TABLES FOUND. DELETING IMAGE.')
      END
      SUBROUTINE FITHDR (IOP, KVOL, ISLOT, NODATA, IERR)
C-----------------------------------------------------------------------
C   FITHDR reads the tape which must be open and positioned at begin.
C   of file) and builds a catalog header and pointers from the
C   tape header records.  After the required fits cards are read a
C   map file with a temporary name is created and the history records
C   are recognized and written to the history file as the other header
C   cards are processed.  The file is later renamed to the correct name.
C   Inputs:
C     IOP    I     Operation code 1=> read tape, 2=>just create dummy
C                  file
C     KVOL   I     Disk volume for cataloged map.
C   Output:
C     ISLOT  I     Catalog slot number for new map file.
C     NODATA L     True if tape contains no data section, else false.
C     ERR    I     =0 => ok
C                   other => quit
C-----------------------------------------------------------------------
      CHARACTER LINE*80, DASH*4, TEMP*4, TCLASS*4, CHTEMP*8
      REAL      PIX11(2)
      INTEGER   IOP, IWIN(4), NBLKOF, ICARD, IE, IST, IERR, ISLOT, IREC,
     *   I, IN, IS, IAX, IDEPTH(5), ICEND, KVOL
      LOGICAL   END, F, T, ISHIST, NODATA, DOHI
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DASH /'----'/
      DATA IWIN, NBLKOF /0,0,0,0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA TEMP, TCLASS /'IMLO','TEMP'/
C-----------------------------------------------------------------------
C                                       Initialize BLANK values flag
C                                       to false.
      NODATA = .FALSE.
      ISBLNK = .FALSE.
      FUCKUP = .FALSE.
      CALL CATCLR (CATBLK)
C                                       See if tape read requested
      IF (IOP.EQ.2) GO TO 15
C                                       Go to beginning of file
         CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       allow blocked tapes
         FDVEC(6) = 10
         FDVEC(32) = 0
C                                       Initialize header values.
         CALL CATINI (CATBLK)
         SCALE = 1.0D0
         OFFSET = 0.0D0
         ISCALE = 1.0D0
         IZERO = 0.0D0
C                                       Read record 1 from tape.
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       Decode required cards.
         CALL IMREQC (HDRBUF, ICEND, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (CATBLK(KIDIM).NE.0) GO TO 5
C                                       No data, must have tables.
            IF (.NOT.STDEXT) GO TO 980
C                                       Make a 2x2 map.
 15         NODATA = .TRUE.
            ISBLNK = .TRUE.
            CATBLK(KIDIM) = 2
            CATBLK(KINAX) = 2
            CATBLK(KINAX+1) = 2
C                                       More defaults.
 5    DO 10 I = 1,KICTPN
         CATR(KRCRP+I-1) = CATBLK(KINAX+I-1) / 2
         CATR(KRCIC+I-1) = 1.0
 10      CONTINUE
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'GSCAT       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
C      CALL MCREAT (KVOL, CNO, HBUFF, IERR)
C                                       Blank name back out.
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
      ISLOT = CNO
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      FVOL(NCFILE) = KVOL
C                                       See if need to parse rest of
C                                       header.
      IF (IOP.EQ.2) GO TO 210
C
         ICARD = ICEND + 1
C                                       Loop until END card found.
         DO 90 IREC = 1,1000000
C                                       Read next record.
            IF (ICARD.LE.36) GO TO 80
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
               ICARD = 1
C                                       Parse card, put value in hdr.
 80         CALL IMPARS (ICARD, HDRBUF, ISHIST, END, IERR)
            IF (END) GO TO 100
C                                       Add to history file.
            IF (IERR.EQ.0) GO TO 85
               IST = 80 * ICARD - 79
               CHTEMP = HDRBUF(IST:)
               DOHI = (CHTEMP.EQ.'HISTORY') .OR. (CHTEMP.EQ.'COMMENT')
     *            .OR. (CHTEMP.EQ.' ')
               LINE = HDRBUF(IST:)
               IST = 1
               IF (DOHI) IST = IST + 8
 85         ICARD = ICARD + 1
 90         CONTINUE
C                                       Read more cards than we expected
         WRITE (MSGTXT,1090)
         GO TO 990
C                                       End card found.
 100     CONTINUE
C                                       Make axis increments non zero
C                                       to help out dumb programs.
         IN = KINAX
         IS = KRCIC
         IE = IS + CATBLK(KIDIM) - 1
         DO 200 IAX = IS,IE
            IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1))
     *         CATR(IAX) = 1.0
            IN = IN + 1
 200        CONTINUE
 210  CONTINUE
C                                       Correct for PDP 11 values.
C                                       set common values
      IF (FUCKUP) THEN
         DO 310 I = 3,7
            IDEPTH(I-2) = 1
            IF (I.LE.CATBLK(KIDIM)) THEN
               IDEPTH(I-2) = CATR(KRCRP+I-1) + 0.5
               IDEPTH(I-2) = MAX (1, MIN (IDEPTH(I-2),
     *            CATBLK(KINAX+I-1)))
               END IF
 310        CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
C                                       do conversion
         IF ((ABS(POS11(1)-CATD(KDCRV+KLOCL(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCL(LOCNUM)))) .OR.
     *      (ABS(POS11(2)-CATD(KDCRV+KLOCM(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCM(LOCNUM))))) THEN
            CALL LMPIX (POS11(1), POS11(2), PIX11(1), PIX11(2))
            IF ((ABS(PIX11(1)-CATR(KRCRP+KLOCL(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCL(LOCNUM))/2) .OR.
     *         (ABS(PIX11(2)-CATR(KRCRP+KLOCM(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCM(LOCNUM))/2)) THEN
               WRITE (MSGTXT,1310) POS11
               CALL MSGWRT (6)
               WRITE (MSGTXT,1311) PIX11
               CALL MSGWRT (6)
            ELSE
               WRITE (MSGTXT,1320) CATR(KRCRP+KLOCL(LOCNUM)),
     *            CATR(KRCRP+KLOCM(LOCNUM)), PIX11
               CALL MSGWRT (3)
               CATR(KRCRP+KLOCL(LOCNUM)) = PIX11(1)
               CATD(KDCRV+KLOCL(LOCNUM)) = POS11(1)
               CATR(KRCRP+KLOCM(LOCNUM)) = PIX11(2)
               CATD(KDCRV+KLOCM(LOCNUM)) = POS11(2)
               END IF
            END IF
         END IF
      GO TO 999
C                                       No Data, no tables.
 980  WRITE (MSGTXT,1980)
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAP CREATE ERROR')
 1090 FORMAT ('READ MORE THAN 32767 CARDS WITHOUT FINDING AN END CARD')
 1310 FORMAT ('PDP11/70 ERROR: PHASE REF. POS.',2E14.6)
 1311 FORMAT ('GIVES REF. PIXEL',2F9.2,' IGNORED')
 1320 FORMAT ('CORRECTING REF. PIXEL FROM',2F8.2,' TO',2F8.2)
 1980 FORMAT ('THIS FITS FILE HAS NEITHER DATA NOR TABLES')
      END
      SUBROUTINE IMREQC (FITBLK, ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:
C      FITBLK  C*2880   a block of fit header data.
C   Outputs:
C      ICARD   I        The number of the last card parsed.
C      IERR    I        0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, EXTEND*8, KL*80
      INTEGER   NPNT, ITYP, NAXIS, ITABNO, IVAL, IKEYWD, I, IAX
      LOGICAL   ISHIST, END
      DOUBLE PRECISION X
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:VFUV.INC'
      DATA EXTEND /'EXTEND  '/
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.) IVAL = X + 0.1
      IF (X.LT.0.) IVAL = X - 0.1
      TAPEBP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32)) GO TO 950
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C
      IAX = KINAX
      CATBLK(KIDIM) = NAXIS
C                                       Check for invalid no. of axis
C                                       for our header.
      IF (NAXIS.GT.7) GO TO 960
C                                       Check NAXISm
      DO 30 I = 1,NAXIS
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT,
     *      KL, SYMBOL, ITABNO, ISHIST, END, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GETNUM (KL, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         CATBLK(IAX) = X + .01
         IAX = IAX + 1
 30      CONTINUE
C                                       Look for EXTEND = T card.
      IF (CATBLK(KINAX).EQ.0) GO TO 930
      ICARD = ICARD + 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, EXTEND, FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, ITYP)
      IF (END.OR.ISHIST.OR.(ITYP.NE.0)) GO TO 50
          CALL GETLG (KL, 80, NPNT, ITYP)
          IF (ITYP.NE.1) GO TO 50
          STDEXT = .TRUE.
          GO TO 999
 50   CONTINUE
C                                       No extensions
         ICARD = ICARD - 1
         STDEXT = .FALSE.
      GO TO 999
C                                       Probably a UV tape.
 930  WRITE (MSGTXT,1930)
      CALL MSGWRT (8)
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'IMREQC: BAD NUMBER OF ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('THIS IS PROBABLY A UV DATA FILE THAT MUST BE READ WITH',
     *   ' UVLOD')
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE IMPARS (ICARD, FITBLK, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C   IMPARS (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      ISHIST  L         True iff a history card
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error.
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      LOGICAL   ISHIST, END
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, STR*68, KL*80
      DOUBLE PRECISION X
      REAL      VAL
      LOGICAL   LHIST, FIRST
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNSTR, NPNTS
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFIT.INC'
C-----------------------------------------------------------------------
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      NN = NKT + NCT
      NNSTR = NCT + 1
C                                       Loop for all possible values
C                                       on an AIPS HISTORY card.
      FIRST = .TRUE.
 10   CONTINUE
      CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF (IERR.NE.0) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
C      IF (PNTR.EQ.0) GO TO 999
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
      NPNTS = NPNT
      GO TO (100, 200, 300), KT
C                                       Logical value
 100     CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.GE.0) GO TO 110
            WRITE (MSGTXT,1100) SYMBOL
            GO TO 990
C                                       Logical value special cases.
 110     CONTINUE
C                                       Handle normal logical cases.
         CATBLK(PNTR+IPOFF) = IL
         GO TO 400
C                                       Number
 200     CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) GO TO 975
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).NE.'BLANK') GO TO 220
            IF (X.EQ.-2147483648.0D0) THEN
               IBLNK = -2147483647 - 1
            ELSE
               IBLNK = X
               END IF
            ISBLNK = .TRUE.
            GO TO 400
C                                       PDP 11 Stuff
 220     IF ((AWORD(TABNO).NE.'OPHRAE11') .AND.
     *      (AWORD(TABNO).NE.'OPHDCE11')) GO TO 230
            POS11(IPOFF) = X
            FUCKUP = .TRUE.
            GO TO 400
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       2-byte integer
 230     IF (NBYT.NE.2) GO TO 240
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) THEN
               CATBLK(PNTR+IPOFF) = IVAL
            ELSE
               IF (AWORD(TABNO).EQ.'BITPIX') TAPEBP = IVAL
               IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
               END IF
            GO TO 400
C                                       4-byte real
 240     IF (NBYT.NE.4) GO TO 250
            IF ((AWORD(TABNO) .EQ. 'HISTORY') .AND. (X .GT. 1.0E30))
     *         X = 1.0E30
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
            GO TO 400
C                                       8-byte real
 250     IF (NBYT.NE.8) GO TO 400
            IF (PNTR.GT.0) THEN
               CATD(PNTR+IPOFF) = X
            ELSE
               IF (AWORD(TABNO).EQ.'BSCALE') SCALE = X
               IF (AWORD(TABNO).EQ.'ISCALE') ISCALE = X
               IF (AWORD(TABNO).EQ.'BZERO') OFFSET = X
               IF (AWORD(TABNO).EQ.'IZERO') IZERO = X
               END IF
            GO TO 400
C                                       String
 300     CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
         NCHAR = MIN (NBYT, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
C                                       Start string on integer boundary
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CALL CHFILL (NBYT, HBLANK, IPOFF, CATH(PNTR))
            CALL CHR2H (NCHAR, STR, IPOFF, CATH(PNTR))
            GO TO 400
            END IF
C                                       Start string on real boundary.
         IPOFF = (NBYT / 4) * IPOFF
         CALL CHFILL (NBYT, HBLANK, 1, CATH(PNTR+IPOFF))
         CALL CHR2H (NCHAR, STR, 1, CATH(PNTR+IPOFF))
C
C                                       If this is a history card, look
C                                       for more values.
 400     IF (ISHIST) GO TO 10
         GO TO 999
C                                       Error message
 975  MSGTXT = 'IMPARS: BAD NUMBER ON ' // SYMBOL
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (2A4,'LOGICAL VARIABLE HAS ILLEGAL VALUE')
      END
      SUBROUTINE MLDEF (NAMEX, CLASSX, SEQ)
C-----------------------------------------------------------------------
C   MLDEF fills the image name (name,class,seq) with default values.
C   This subroutine is used with GSCAT.
C   Inputs/Output:
C      NAMEX    C*12  Input image name
C      CLASSX   C*6   Input image class
C      SEQ      I     Input image Sequence
C   Outputs:
C      CATBLK(KIIMS) I     Image sequence
C-----------------------------------------------------------------------
      CHARACTER DNAME*12, DCLASS*6, NAMEX*12, CLASSX*6,
     *   NONE*8, STOKES*8, WTYP(5)*4,
     *   STOK(5)*2, STOK2(5)*4, CHTM12*12
      INTEGER   SEQ, STNUM, NAX, I, J, DSEQ
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NONE, STOKES /'NONE    ','STOKES  '/
      DATA WTYP /'MAP ','CMP ','RES ','PNT ','CLN '/
      DATA STOK /'I ','I ','Q ','U ','V '/
      DATA STOK2 /'PPOL','FPOL','PANG','SPIX','OPTD'/
C-----------------------------------------------------------------------
C                                       check type
      IF ((CATBLK(KITYP).LE.1) .OR. (CATBLK(KITYP).GT.4))
     *   CATBLK(KITYP) = 1
C                                       name default
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), DNAME)
      IF (DNAME.NE.' ') GO TO 20
         CALL H2CHR (8, 1, CATH(KHOBJ), CHTM12)
         IF (CHTM12(1:8).EQ.' ') CALL CHR2H (8, NONE, 1, CATH(KHOBJ))
         CALL H2CHR (8, 1, CATH(KHOBJ), DNAME(1:8))
         DNAME(9:12) = ' '
C                                       class default
 20   CALL H2CHR (6, KHIMCO, CATH(KHIMC), DCLASS)
C                                       Stokes value 1st char
      IF (DCLASS.NE.' ') GO TO 30
         STNUM = 2
         NAX = CATBLK(KIDIM)
         DO 25 I = 1,NAX
            J = (I-1)*2 + KHCTP
            CALL H2CHR (8, 1, CATH(J), CHTM12)
            IF (STOKES.EQ.CHTM12(1:8)) STNUM = CATD(KDCRV+I-1) + 1.5
 25         CONTINUE
C                                       clean type : 2-4 chars
         IF (STNUM.LE.5) THEN
            J = CATBLK(KITYP)
            IF ((J.EQ.1) .AND. (CATBLK(KINIT).GT.0)) J = 5
            IF (STNUM.EQ.1) DCLASS = STOK(STNUM)(1:1) // 'BEM  '
            IF (STNUM.NE.1) DCLASS = STOK(STNUM)(1:1) // WTYP(J)(1:3)
     *         // '  '
         ELSE
C                                       Special "Stokes" values
            DCLASS = STOK2(STNUM-5)(1:4) // '  '
            END IF
C                                       sequence number
C                                       fill in cat block
 30   DSEQ = CATBLK(KIIMS)
      CATBLK(KIIMS) = SEQ
C      CALL MAKOUT (DNAME, DCLASS, DSEQ, DCLASS, NAMEX, CLASSX,
C     *   CATBLK(KIIMS))
      CALL CHR2H (12, NAMEX, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASSX, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C
 999  RETURN
      END
      SUBROUTINE MLTAPE (NMPSKP, IERR)
C-----------------------------------------------------------------------
C   MLTAPE sets up for TAPIO and opens the input for GSCAT.
C   Inputs:  NMPSKP      I    Number files to skip
C   Outputs: IERR        I    Error return
C                             0--> okay,  1--> error
C   Uses and build special common /MLTAPE/
C-----------------------------------------------------------------------
      INTEGER   NMPSKP, IERR, IC
      LOGICAL   T
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
C                                       Disk output.
      IF (.NOT.DODISK) GO TO 20
         CALL CHR2H (48, INFILE, 1, FDVEC(7))
         WRITE (MSGTXT,1000) INFILE
         CALL MSGWRT (2)
         FDVEC(1) = 25
         NTAPE = 1
         FDVEC(5) = NTAPE
         GO TO 25
C                                       Tape input
 20      NTAPE = NTAPE4 + 0.5
         IF (NTAPE.LE.0) NTAPE = 1
         FDVEC(1) = 129 - NTAPE
         FDVEC(5) = NTAPE
         WRITE (MSGTXT,1020) NTAPE
         CALL MSGWRT (2)
C                                       Open tape
 25   CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1025) IERR
         GO TO 980
C                                       Advance files
 30   IF (DODISK) GO TO 999
      IC = NMPSKP
      IF (IC.LE.0) GO TO 35
         WRITE (MSGTXT,1030) IC
         CALL MSGWRT (2)
         CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), IC, IERR)
         IF (IERR.EQ.0) GO TO 999
            WRITE (MSGTXT,1031) IERR, 'ADVF'
            GO TO 980
C                                       Back files
 35   CONTINUE
         IC = -IC
         WRITE (MSGTXT,1035) IC
         IF (IC.EQ.0) WRITE (MSGTXT,1036)
         CALL MSGWRT (2)
         IC = IC + 1
         CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IC, IERR)
         IF (IERR.EQ.0) GO TO 999
            WRITE (MSGTXT,1031) IERR, 'BAKF'
C                                       Error returns
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Reading from disk file: ',A)
 1020 FORMAT ('Reading tape drive number ',I3)
 1025 FORMAT ('MLTAPE: COULD NOT OPEN TAPE.  IER=',I7)
 1030 FORMAT ('ADVANCING TAPE BY',I4,' FILES')
 1031 FORMAT ('ERROR',I4,' ON OPERATION ',A4)
 1035 FORMAT ('Moving tape backwards by',I4,' files')
 1036 FORMAT ('Positioning tape at beginning of current file')
      END
      SUBROUTINE MLTABL (VOL, IRET)
C-----------------------------------------------------------------------
C   MLTABL processes records following the normal FITS image.  If
C   TABLES <= 0, it simply counts the number of such records.  Else,
C   it parses through the Table records creating the appropriate
C   extension files and adding the table header cards to the
C   history file.
C   Inputs:  VOL    I         Output disk volume #
C   Output:  IRET   I         Error code: 0 => ok, 8 => some error
C-----------------------------------------------------------------------
      INTEGER   VOL, IRET
C
      CHARACTER ISTR*80, SYM*8, CARD*80, CTYPES(2)*4, LLCHAR*4,
     *   SYMS(15)*8, CHTM12*12, TABNAM*8, TTYPE(10)*8
      INTEGER   TABCNT, IRNO, IERR, TERR, TABVER, TABWID, TABCRD, NC,
     *   NPNT, ITYP, NSYMS, NCHAR, ITAB, NTYPES, IT, LUN, INC,
     *   BUFFER(768), IP, I, IREC, J, NDIM, JJ, IST
      LOGICAL   HISERR, EQUAL, NODATA, T, ISHIS
      REAL      RDATA(10), XINC
      DOUBLE PRECISION    X
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA NSYMS, NTYPES, LUN /15, 2, 28/
      DATA CTYPES,     LLCHAR
     *   /' CCC','  CC','LL  '/
      DATA SYMS /'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ',
     *   'TTYPE5  ', 'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ',
     *   'TTYPE9  ', 'TTYPE10 ', 'TABNAME ', 'TABVER  ',
     *   'TABCOUNT', 'TABWIDTH', 'TABCARDS'/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IERR = 0
      TERR = 0
      IRET = 0
      IF (TABLES.LE.0) GO TO 900
      HISERR = .FALSE.
      IRET = 8
C                                       Loop over tables
      DO 200 ITAB = 1,TABLES
         WRITE (CARD,1000) ITAB
C                                       Init table parm values
         TABVER = 0
         TABCNT = 0
         TABWID = 0
         TABCRD = 0
         TABNAM = '        '
         DO 20 I = 1,10
            TTYPE(I) = '        '
 20         CONTINUE
C                                       Read and parse header
         DO 90 IREC = 1,100
            IF (IREC.EQ.1) GO TO 50
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               IF (TERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1020) TERR
                  GO TO 890
 30            NC = 0
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       card loop
 50         NC = NC + 1
            IF (NC.GT.36) GO TO 90
C                                       card to history
            INC = (NC-1) * 80 + 1
            CARD = HDRBUF(INC:)
            IF (HISERR) GO TO 55
               ISHIS = CARD(1:8).EQ.'HISTORY'
               IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.'COMMENT'
               IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.' '
               IST = 1
               IF (ISHIS) IST = 9
C                                       Parse
 55         NPNT = 1
            CALL GETSYM (CARD, NPNT, SYM, ITYP)
            IF (SYM.EQ.'END ') GO TO 100
C                                       only keyword = value accepted
            IF (ITYP.NE.0) GO TO 50
            DO 60 I = 1,NSYMS
               IF (SYM.EQ.SYMS(I)) GO TO 70
 60            CONTINUE
            GO TO 50
C                                       Numeric keywords
 70         IF (I.LE.11) GO TO 80
               CALL GETNUM (CARD, 80, NPNT, X)
               IF (X.EQ.DBLANK) GO TO 875
               IF (I.EQ.12) TABVER = X + 0.01
               IF (I.EQ.13) TABCNT = X + 0.01
               IF (I.EQ.14) TABWID = X + 0.01
               IF (I.EQ.15) TABCRD = X + 0.01
               GO TO 50
C                                       Got a string variable
 80            CALL GETSTR (CARD, 80, 68, NPNT, ISTR, NCHAR)
               NCHAR = MIN (NCHAR, 8)
               IF (I.EQ.11) TABNAM = ISTR(1:NCHAR)
               IF (I.LT.11) TTYPE(I) = ISTR(1:NCHAR)
               GO TO 50
 90         CONTINUE
         WRITE (MSGTXT,1090) ITAB
         GO TO 890
C                                       END card found
C                                       null table
 100     IF ((TABCNT.GT.0) .AND. (TABWID.GT.0)) GO TO 105
            WRITE (MSGTXT,1100) ITAB
            CALL MSGWRT (6)
            GO TO 170
C                                       illegal format
 105     IF ((TABCRD.GT.0) .AND. (TABCRD.LE.40)) GO TO 110
            WRITE (MSGTXT,1105) TABCRD, ITAB
            GO TO 890
C                                       A recognized type?
 110     NODATA = .TRUE.
         IF (TABNAM(1:4).NE.'AIPS') GO TO 125
            DO 115 IT = 1,NTYPES
               IF (CTYPES(IT).EQ.TABNAM(5:8)) GO TO 120
 115           CONTINUE
            GO TO 125
C                                       Yes: do it - CC files only
 120     IF ((IT.NE.1) .AND. (IT.NE.2)) GO TO 125
            NODATA = .FALSE.
C                                       Set correction for old CC
            XINC = 0.0
            IF (IT.NE.2) GO TO 124
               NDIM = CATBLK(KIDIM)
               DO 122 I = 1,NDIM
                  J = (I-1) * 2 + KHCTP
                  CALL H2CHR (4, 1, CATH(J), CHTM12)
                  EQUAL = LLCHAR(1:4).EQ.CHTM12(1:4)
                  IF (EQUAL) XINC = CATR(KRCIC+I-1)
 122              CONTINUE
               WRITE (MSGTXT,1122)
               CALL MSGWRT (2)
 124        CALL CCINI (LUN, TABWID, VOL, CNO, TABVER, CATBLK, BUFFER,
     *         IERR)
            IF (IERR.EQ.0) GO TO 125
               NODATA = .TRUE.
               WRITE (MSGTXT,1124) IERR
               CALL MSGWRT (7)
 125     IP = TABCRD
         DO 160 IRNO = 1,TABCNT
            DO 150 J = 1,TABWID
               IP = IP + 1
               IF (IP.LE.TABCRD) GO TO 140
                  NC = NC + 1
C                                       read a record
                  IF (NC.LE.36) GO TO 130
                     NC = 1
                     CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
                     CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
                     IF (TERR.EQ.0) GO TO 130
                        WRITE (MSGTXT,1020) TERR
                        GO TO 890
C                                       spread new card
 130              IP = 1
                  IF (NODATA) GO TO 140
                     INC = (NC-1) * 80 + 1
                     CARD = HDRBUF(INC:)
                     NPNT = 1
 140              IF (NODATA) GO TO 150
                     CALL GETNUM (CARD, 80, NPNT, X)
                     IF (X.EQ.DBLANK) GO TO 875
                     JJ = J
C                                       format correction
                     IF (IT.EQ.1) GO TO 145
                        IF (J.EQ.1) X = X + XINC
                        IF (J.LE.3) JJ = MOD (J, 3) + 1
 145                 RDATA(JJ) = X
 150              CONTINUE
 160           CONTINUE
 170        CONTINUE
C                                       Data ignored
 200     CONTINUE
      IRET = 0
      GO TO 900
C
 875  MSGTXT = 'MLTABL: BAD NUMBER ON ' // SYM
      IRET = 1
 890  CALL MSGWRT (8)
C                                       Read rest of tape
 900  IF (TERR.NE.4) CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
      IF (IERR.NE.0) IRET = 6
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GSCAT  / HEADER FOR TABLE',I7)
 1020 FORMAT ('MLTABL: TAPE IO ERROR',I7)
 1090 FORMAT ('MLTABL: NO END TO TABLE HEADER #',I7)
 1100 FORMAT ('TABLE',I7,' HAS NO DATA')
 1105 FORMAT ('MLTABL: TABCARDS=',I7,' ILLEGAL')
 1122 FORMAT ('Correcting old CC format X positions')
 1124 FORMAT ('MLTABL: UNABLE TO CREATE EXTENSION FILE',I7)
      END
      SUBROUTINE FITRXM (HLUN, HBUFF, NUMTAB, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with a map
C  and process the ones it recognizes (XTENSION = 'TABLES' as of now).
C  Inputs:
C     HLUN    I       History file LUN. Already open.
C     HBUFF   I(256)  History file I/O buffer.
C  Outputs:
C     NUMTAB  I       Number of extension files found.
C     EOF     L       An end of file was read during processing.
C     IERR    I       Error code. 0=ok. >0 => Error
C                     -1 => too many tables for one output file.
C-----------------------------------------------------------------------
      INTEGER   MXTBKW, MAXTAB
C                                       MXTBKW=max. no. table keywords
      PARAMETER (MXTBKW = 1000)
C                                       MAXTAB = max number of tables.
      PARAMETER (MAXTAB=250)
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TABLE*8, TAB3D(3)*8
      INTEGER   HLUN, HBUFF(256), IERR, NUMTAB
      LOGICAL   EOF, T
      DOUBLE PRECISION    NBITS, AXCNT, KEYVAL(MXTBKW), KEYD
      REAL      KEYR(2), RA, DEC
      HOLLERITH KEYH(2)
      INTEGER   I, II, J, L, IP, LP, ICARD, INBLK, KEYTYP(MXTBKW), IVER,
     *   TABLUN, SRTORD, DATP(128,2), BUFFER(1024), NUMKEY, KEYI, IKEY,
     *   KEYLOC(MXTBKW), KEYV(2*MXTBKW), LENKEY(5), REGION, TTCODE(60)
      LOGICAL   EXTEN, KEYL, DOHDR
      INCLUDE 'GSCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TABLE, TAB3D /'TABLE', 'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA TABLUN /29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
C                                       Open history
      NUMTAB = 0
C                                       Loop for all FITS extensions.
      DO 200 I = 1,1000000
C                                       Process all required FITS table
C                                       cards.
         CALL EXTREQ (FDVEC, TBIND, TAPBUF, HDRBUF, ICARD, EXTEN, EOF,
     *      UNKNWN, IERR)
         IF (EOF) GO TO 900
         IF ((IERR.NE.0) .OR. (.NOT.EXTEN)) GO TO 900
C                                       Calculate no. of data blocks.
         AXCNT = 1.0D0
         DO 20 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 20         CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = ABS (BITPIX) * GCOUNT * AXCNT
         INBLK = INT ((NBITS + 23039.0D0) / 23040.0D0)
         DOHDR = .FALSE.
C                                       See if we have an ASCII table.
         IF (EXTTYP.NE.TABLE) GO TO 40
            IF (NAXISI(1).LE.2880) GO TO 25
               WRITE (MSGTXT,1020) NAXISI(1)
               CALL MSGWRT (6)
               GO TO 100
C                                       initialize default values.
 25         CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 0, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, HDRBUF, IERR)
            IF (IERR.LT.0) GO TO 100
C                                       Normal table files.
            SRTORD = 0
            IVER = 0
C                                       Start MAKTAP functions
C                                       Zero out control block.
            CALL FILL (256, 0, DATP)
C                                       Ensure small integer data
C                                       types don't get transferred
C                                       to output table type array
            CALL FILL (60, 0, TTCODE)
            DO 27 J = 1, ITNCOL
               TTCODE(J) = TFCODE(J)
               IF (MOD(TFCODE(J),10).EQ.6) TTCODE(J) = TFCODE(J) - 2
 27            CONTINUE
            CALL COPY (ITNCOL, TTCODE, DATP(1,2))
C                                       Start TABINI functions
            IP = 1
            LP = 1
            DO 29 II = 1,7
               IF (II.EQ.6) GO TO 29
               DO 28 J = 1,ITNCOL
C                                       Found column of right type
                  IF (MOD(DATP(J,2), 10).EQ.II) THEN
C                                       DATP(J,1) = Pointer in array
C                                       of appropriate type.
                     DATP(J,1) = IP
                     BUFFER(128+J) = LP
                     LP = LP + 1
C                                       Get length of array.
                     L = DATP(J,2) / 10
                     IF (II.EQ.3) L = (L-1) / 4 + 1
                     IF (II.EQ.7) L = (L-1) / NBITWD + 1
                     IF (DATP(J,2).LT.10) L = 0
C                                       Set pointer for next entry.
                     IP = IP + L
C                                       If L>1 and I .NE. 3 then the
C                                       file cannot be written as
C                                       FITS ASCII
                     IF ((L.GT.1) .AND. (II.NE.3)) BUFFER(60) = 1
                     END IF
 28              CONTINUE
C                                       Set pointer for next type.
             IF (II.EQ.1) IP = (IP-1) * NWDPDP + 1
 29         CONTINUE
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 30 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.2) KEYR(1) = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) CALL CHR2H (8, KEYCHR(II), 1, KEYH)
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 30            CONTINUE
C                                       Write keywords.
C            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
C     *         KEYV, KEYTYP, JERR)
C                                       Read the data from tape and
C                                       write to the table disk file.
            RA  = SHIFT(1)
            DEC = SHIFT(2)
            CALL GSTAB (FDVEC, TBIND, DATP, NAXISI, RA, DEC, TAPBUF,
     *         REGION, IERR)
            IF (IERR.NE.0) GO TO 900
            NUMTAB = NUMTAB + 1
            GO TO 190

 40      CONTINUE
 100     CONTINUE
C                                       read rest header code
            DOHDR = .TRUE.
C                                       else header already read
            CALL SKPEXT (DOHDR, FDVEC, TBIND, HLUN, ICARD, INBLK, HBUFF,
     *         TAPBUF, HDRBUF, IERR)
C                                       Quit if filled up tables.
 190     IF (EOF) GO TO 900
C                                       Change /CFILES/ not to destroy
C                                       on ERROR
         FRW(NCFILE) = 1
         NCFILE = 0
 200     CONTINUE
C                                       Shouldn't get here.
      WRITE (MSGTXT,1200)
      CALL MSGWRT (6)
C                                       Close history
 900  CONTINUE
C                                       Trap too many tables
      IF ((IVER.GT.MAXTAB) .AND. (IERR.EQ.0)) IERR = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FITRXM: ASCII TABLE ROW LENGTH',I7,' TOO LONG FOR ME')
 1200 FORMAT ('MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED.')
      END
      SUBROUTINE GSTAB (FDVEC, TBIND, DPTR, NAXIS, RA, DEC, TAPBUF,
     *   REGION, IERR)
C-----------------------------------------------------------------------
C  This routine will read the data section of a FITS extension file
C  of type TABLE and decode this information using data obtained from
C  the header section of the extension file, and write the AIPS version
C  of a table file.  Limited to tables lines <= 2880 characters.
C  Inputs:
C     FDVEC    I(50)      File descriptor vector for TAPIO input
C     DPTR     I(128,2)   Data Pointers, used in table file control.
C     INBLK    I          Number of blocks on tape for table file.
C     NAXIS    I(2)       Length of columns (in char), number of rows.
C     RA       R          RA of region needed
C     DEC      R          Dec of region needed
C  In/Out:
C     TBIND    I          Buffer pointer in TAPBUF
C     IBLK     I(>=512)     Disk Table file I/O buffer.
C     TAPBUF   I(*)       Tape I/O buffer.
C  Outputs:
C     REGION   I          Region number
C     IERR     I          Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER TLINE*2880
      HOLLERITH RECH(1)
      REAL      RA, DEC
      INTEGER   NAXIS(2), FDVEC(50), TBIND, DPTR(128,2), TAPBUF(*),
     *   REGION, IERR
      DOUBLE PRECISION X, RECRD(1)
      CHARACTER*1 DECSIL, DECSIH
      REAL      RECRR(512), RALOW, RAHI, DECLOW, DECHI, TMPHI, TMPLOW
      INTEGER   NAXIS2, LCTR, RECII(1), IT(256), ILEN(256), NCNTR,
     *   I, NAXIS1, IOFF, IT0, NPRNT
      LOGICAL   NULL, RECLL(1)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECII, RECLL, RECRD, RECRR, RECH)
C-----------------------------------------------------------------------
      NPRNT = 0
      REGION= 0
      NCNTR = 2881
C                                       Calculate end & type
C                                       of Column values.
C                                       Use TFCODE to determine types
C                                       because DPTR holds output info.
      DO 10 I = 1,ITNCOL
         IT(I) = MOD (TFCODE(I),10)
C                                       Length of Character types in
C                                       reals.
         IF (IT(I).EQ.3) ILEN(I) = ((DPTR(I,2) / 10) + 3) / 4
 10      CONTINUE
C                                       Loop for all lines in table.
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
      DO 800 LCTR = 1,NAXIS2
C                                       Read a FITS table data line.
         CALL TABLIN (NAXIS1, FDVEC, TBIND, NCNTR, TAPBUF, TLINE, IERR)
         IF (IERR.NE.0) GO TO 999
         DO 200 I = 1,ITNCOL
C                                       Decode the data fields in this
C                                       row.
C                                       Look for null values.
            NULL = TLINE(TBCOL(I):TBCOL(I)+TWIDTH(I)-1) .EQ.
     *         TNULL(I)(1:TWIDTH(I))
C                                       Go to correct type
            IOFF = DPTR(I,1)
            IT0 = IT(I)
            GO TO (110, 120, 130, 150, 160, 150, 170), IT0
C                                       Double precision
 110           IF (.NOT.NULL) THEN
                  CALL DCODEF (TBCOL(I), TWIDTH(I), TFRAC(I), TLINE,
     *               X, IERR)
                  RECRD(IOFF) = X
               ELSE
                  RECRD(IOFF) = DBLANK
                  END IF
               GO TO 200
C                                       Single precision
 120           IF (.NOT.NULL) THEN
                  CALL DCODEF (TBCOL(I), TWIDTH(I), TFRAC(I), TLINE,
     *               X, IERR)
                  RECRR(IOFF) = X
               ELSE
                  RECRR(IOFF) = FBLANK
                  END IF
               GO TO 200
C                                       Character.
 130           IF (.NOT.NULL) THEN
                  CALL CHR2H (TWIDTH(I), TLINE(TBCOL(I):), 1,
     *               RECH(IOFF))
               ELSE
                  CALL CHFILL (TWIDTH(I), HBLANK, 1, RECH(IOFF))
                  END IF
C                                       If Dec Sign LOW
               IF (I.EQ.8)  DECSIL = TLINE(TBCOL(I):TBCOL(I))
C                                       If Dec Sign HI
               IF (I.EQ.11) DECSIH = TLINE(TBCOL(I):TBCOL(I))
               GO TO 200
C                                       Integer
 150           IF (.NOT.NULL) THEN
                  CALL DCODEF (TBCOL(I), TWIDTH(I), TFRAC(I), TLINE,
     *               X, IERR)
                  IF (X.GT.0.0D0) THEN
                     RECII(IOFF) = X + 0.5
                  ELSE
                     RECII(IOFF) = X - 0.5
                     END IF
               ELSE
                  RECII(IOFF) = BLANKV
                  END IF
               GO TO 200
C                                       Logical
 160           RECLL(IOFF) = TLINE(TBCOL(I):TBCOL(I)).EQ.'T'
               GO TO 200
C                                       Hexbit
C                                       Not supported.
 170        CONTINUE
C
 200        CONTINUE
C                                       Unpack the results
C                                       Calculate the Ra and Dec
C                                       Hours
         RALOW =  RECII(DPTR(2,1))
C                                       Minutes
         X = RECII(DPTR(3,1))
         RALOW = RALOW + (X/60.)
C                                       Seconds
         X = RECRR(DPTR(4,1))
         RALOW = RALOW + (X/3600.)
C                                       Hours
         RAHI  =  RECII(DPTR(5,1))
C                                       Minutes
         X = RECII(DPTR(6,1))
         RAHI  = RAHI  + (X/60.)
C                                       Seconds
         X = RECRR(DPTR(7,1))
         RAHI  = RAHI  + (X/3600.)
C                                       Dec Degrees
         DECLOW =  RECII(DPTR(9,1))
C                                       Minutes
         X = RECRR(DPTR(10,1))
         DECLOW = DECLOW + (X/60.)
C                                       Dec Sign
         IF (DECSIL.EQ.'-') DECLOW = -DECLOW
C                                       Dec Degrees
         DECHI  =  RECII(DPTR(12,1))
C                                       Minutes
         X = RECRR(DPTR(13,1))
         DECHI  = DECHI  + (X/60.)
C                                       Dec Sign
         IF (DECSIH.EQ.'-') DECHI  = -DECHI
C                                       Fixed messed up table sort
         TMPLOW = DECLOW
         TMPHI  = DECHI
         DECLOW = MIN(TMPLOW,TMPHI)
         DECHI  = MAX(TMPLOW,TMPHI)
C                                       On first, print label
         IF (LCTR.EQ.1) THEN
            MSGTXT = 'GSTAB:  Ra(hr)  Dec(deg) '
            CALL MSGWRT(2)
            END IF
C                                       Is RA,DEC in region
         IF (RA .GE.RALOW  .AND. RA.LE.RAHI .AND.
     *       DEC.GE.DECLOW .AND. DEC.LE.DECHI) THEN
C                                       Count number of regions
            NPRNT = NPRNT + 1
C                                       Record region
            REGION = RECII(DPTR(1,1))
            MSGTXT = '--------------------------------------------'
            CALL MSGWRT(2)
C                                       Print input
            WRITE (MSGTXT,1000) RA, DEC, REGION
            CALL MSGWRT(4)

            WRITE(MSGTXT,1100) RAHI, DECHI
            CALL MSGWRT(4)
            WRITE(MSGTXT,1200) RALOW, DECLOW
            CALL MSGWRT(4)
C                                       Exit if printed enough
            IF (NPRNT.GT.3) GO TO 990
C                                       End if in region
            END IF
 800     CONTINUE
C
      IF (REGION.LE.0) THEN
         WRITE (MSGTXT,2000) RA, DEC
         CALL MSGWRT(5)
         IERR = 1
         END IF
 990  MSGTXT = '--------------------------------------------'
      CALL MSGWRT(2)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GSTAB: ',F8.3,',',F8.3,' in region ',I6)
 1100 FORMAT ('GSTAB: ',F8.3,',',F8.3,' is region Maximum ')
 1200 FORMAT ('GSTAB: ',F8.3,',',F8.3,' is region Minimum ')
 2000 FORMAT ('GSTAB: ',F8.3,',',F8.3,' NOT FOUND IN TABLE ')
      END
