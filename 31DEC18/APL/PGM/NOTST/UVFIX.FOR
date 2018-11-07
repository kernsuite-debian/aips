LOCAL INCLUDE 'UVFIX.INC'
C                                       Local include for UVFIX
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXINT, MAXPOI, MAXFIT, MAXSOU
      PARAMETER (MAXFIT = 5)
      PARAMETER (MAXINT = 2000)
      PARAMETER (MAXPOI = MAXINT * MAXFIT)
      PARAMETER (MAXSOU = 2000)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, SHIFT(2), XSOUT, XDISO, APARM(10),
     *   BPARM(10), XINV, XCENT, BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      INTEGER   JBUFSZ, NDOPER
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, SHIFT, APARM, BPARM, XINV, XCENT
C
      INTEGER   CATOLD(256), SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   NUMSUB, INFILE, OUTFIL, OLDCNO, RNXRET
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64, BNDCOD(MAXIF,MAXFQC)*8
      DOUBLE PRECISION UVSCAL
      COMMON /UVFIXP/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NUMHIS, ISCMP, NUMSUB, INFILE, OUTFIL, OLDCNO, NDOPER, DIFPIX
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD, BNDCOD
C                                                    new variables
      INTEGER   NIF, NCH, NST, ISBAND(MAXIF,MAXFQC), FID, JNCIF, JNCF,
     *   JNCS
      REAL      FINC(MAXIF,MAXFQC), REFCHN, INEPO
      DOUBLE PRECISION FOFF(MAXIF,MAXFQC), REFF, SRA(MAXSOU),
     *   SDEC(MAXSOU), SRAPP(MAXSOU), SDAPP(MAXSOU), SFREQO(MAXSOU),
     *   INRA, INDEC, INORA, INODE, INRA0, INDE0
      COMMON /VISCOM/ FOFF, REFF, SRA, SDEC, SRAPP, SDAPP, SFREQO, INRA,
     *   INDEC, INORA, INODE, INRA0, INDE0, INEPO, NIF, NCH, NST, FINC,
     *   ISBAND, FID, JNCIF, JNCF, JNCS, REFCHN, RNXRET
C
      DOUBLE PRECISION TBEG(MAXINT), TEND(MAXINT), AAX(MAXPOI),
     *   AAY(MAXPOI), AAZ(MAXPOI)
      INTEGER   NINTER, NFIT, KFIT(MAXINT)
      LOGICAL   ISCMP, NOOB, ISMULT, ISGMRT, DODOPP
      COMMON /OBTAB/ TBEG, TEND, AAX, AAY, AAZ, NINTER, NFIT, KFIT,
     *   NOOB, ISMULT, ISGMRT, DODOPP
LOCAL END
      PROGRAM UVFIX
C-----------------------------------------------------------------------
C! Recomputes u, v, and w with tangent point shift
C# UV Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000-2001, 2003, 2005-2016
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
C   UVFIX computes the values of u, v, w for a portion of a data
C   base using user provided information and information from the
C   AN (antenna) table.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      SHIFT(2)       SHIFT         Position shift in asec (RA, Dec).
C      UVFIPARM(20)   APARM         array geometry.
C                                   1 => array, 3=other
C                                   2 => array number, 0 means the first
C                                   3-4 => array BX, meters
C                                   5-6 => array BY, meters
C                                   7-8 => array BZ, meters
C                                   9 => 0: correct freq. scaling
C                                     =/ 0: faster but incorrect
C                     BPARM         Time and frequency information.
C                                   1 => UT1-UTC (sec)
C                                   2 => IAT-UTC (sec) def year-1961 sec
C                                   3 => Clock error, 5 sec for VLA.
C                                   4 => Frequency (Hz), .LE.0 means use
C                                        AN file value.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, LOOP, IROUND, LIM1, LIM2, TVER
      INCLUDE 'UVFIX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'UVFIX '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      IRET = 0
      CALL UVWIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Recompute u, v, w, loop over
C                                       subarray.
      TVER = IROUND (APARM(2))
C                                       All subarrays
      IF (TVER.LE.0) THEN
         LIM1 = 1
         LIM2 = NUMSUB
C                                       1 subarray
      ELSE
         LIM1 = TVER
         LIM2 = TVER
         END IF
      DO 100 LOOP = LIM1,LIM2
         APARM(2) = LOOP
         WRITE (MSGTXT,1000) LOOP
         CALL MSGWRT (4)
         IF (ISCMP) CALL CMPUVW (IRET)
         IF (.NOT.ISCMP) CALL SENUVW (IRET)
 100     CONTINUE
C                                       History
      APARM(2) = TVER
      IF (IRET.EQ.0) CALL UVWHIS
      IF (NDOPER.GT.0) THEN
         WRITE (MSGTXT,1100) NDOPER
         CALL MSGWRT (8)
         END IF
C                                       close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Processing subarray ',I4)
 1100 FORMAT ('THERE WERE',I12,' MISSING DOPPLER DATA POINTS')
      END
      SUBROUTINE UVWIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   UVWIN gets input parameters for UVFIX and creates and output file
C   if necessary.
C   Inputs:
C      PRGM     C*6   Program name
C   Outputs:
C      JERR     I     Error code. 0=>OK, otherwise error.
C   Outputs in common:
C      INFILE   I     DFIL.INC file number of input
C      OUTFIL   I     DFIL.INC file number of output
C   See prologue comments in UVFIX for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   JERR
C
      INTEGER   IROUND, NPARM, LUN1, LUN2, NONOT, IERR, LUNI, I, J,
     *   BUFFER(512), INVER, OBTOT, INCX
      CHARACTER NOTTYP(1)*2, STAT*4, UTYPE*2, PTYPE*8
      LOGICAL   T, WASOLD
      INCLUDE 'UVFIX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T /.TRUE./
      DATA LUN1, LUN2, LUNI /27, 28, 29/
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      RNXRET = 0
      NDOPER = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 38
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      JERR = 5
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      CALL COPY (256, CATOLD, CATBLK)
C                                       Get uv header info.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      FOFF(1,1) = 0.0D0
C                                       Is this NCP?
      IF ((TYPUVD.LT.0) .AND. (BPARM(10).LE.0.0)) THEN
         MSGTXT = 'WARNING: CHANGING -NCP (U,V) TO -SIN (U,V,W)'
         CALL MSGWRT (7)
         J = CATBLK(KIPCN)
         DO 10 I = 1,J
            CALL H2CHR (8, 1, CATH(KHPTP+(I-1)*2), PTYPE)
            IF (PTYPE(5:).EQ.'-NCP') THEN
               PTYPE(5:) = '-SIN'
               CALL CHR2H (8, PTYPE, 1, CATH(KHPTP+(I-1)*2))
               END IF
 10         CONTINUE
         TYPUVD = 0
         END IF
      IF (BPARM(10).GT.0.0) THEN
         MSGTXT = 'WARNING: not changing U, V, W'
         CALL MSGWRT (6)
         END IF
C                                       Doppler possible?
      CALL FNDEXT ('FO', CATBLK, NUMSUB)
      IF (NUMSUB.LE.0) CALL FNDEXT ('CL', CATBLK, NUMSUB)
      DODOPP = NUMSUB.GT.0
C                                       Is this a multi-source file
      CALL FNDEXT ('SU', CATBLK, NUMSUB)
      ISMULT = (NUMSUB.GT.0) .AND. (ILOCSU.GE.0)
      IF ((ISMULT) .AND. ((SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)))
     *   THEN
         MSGTXT = 'SHIFTS ARE NOT ALLOWED WITH MULTI-SOURCE FILES'
         CALL MSGWRT (8)
         SHIFT(1) = 0.0
         SHIFT(2) = 0.0
         END IF
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      IF (NUMSUB.EQ.0) THEN
         MSGTXT= 'UVWIN: NO ANTENNA TABLES FOUND'
         GO TO 990
         END IF
C                                       Save old position in header.
      IF (ABS (CATD(KDORA)).LT.1.0E-20) CATD(KDORA) = RA
      IF (ABS (CATD(KDODE)).LT.1.0E-20) CATD(KDODE) = DEC
C                                       save input coordinates
      INRA = RA
      INDEC = DEC
      INRA0 = RA
      INDE0 = DEC
      INORA = CATD(KDORA)
      INODE = CATD(KDODE)
      INEPO = CATR(KREPO)
      IF ((SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)) THEN
         IF (COS(DG2RAD*DEC).NE.0.0D0) RA = RA + SHIFT(1) / (3600.
     *      * COS(DG2RAD * DEC))
         DEC = DEC + SHIFT(2) / 3600.
         INRA = RA
         INDEC = DEC
C                                       Update CATBLK.
         CATD(KDCRV+JLOCR) = RA
         CATD(KDCRV+JLOCD) = DEC
         END IF
C                                       Packed uv data ?
      ISCMP = CATBLK(KINAX).EQ.1
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         FREQ = CATD(KDCRV+JLOCF)
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Fit polinomials to the OB table
C                                       data
      IF (BPARM(6).GT.0.0) THEN
         INVER = BPARM(5) + 0.1
         INVER = MAX (INVER, 1)
C                                       determine number of OB tables
         CALL FNDEXT ('OB', CATBLK, OBTOT)
         IF (OBTOT.LT.INVER) THEN
            WRITE (MSGTXT,1200)
            GO TO 990
            END IF
         CALL OBFIT (BUFFER, OLDCNO, CATBLK, LUNI, INVER, IERR)
         END IF
C                                       If sort order not 'BT' or 'TB'
C                                       mark '**'
      IF ((ISORT.NE.'BT') .AND. (ISORT.NE.'TB'))
     *   CALL CHR2H (2, '**', 1, CATH(KITYP))
      IF (ISORT.NE.'TB') DODOPP = .FALSE.
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      WASOLD = .FALSE.
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
C                                        Update existing CATBLK
      IF (IERR.EQ.2) THEN
         WASOLD = .TRUE.
         FRW(NCFILE+1) = 2
         CALL CATIO ('WRIT', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
      ELSE IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         JERR = 8
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      OUTFIL = NCFILE
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                        Put input file in READ
      UTYPE = 'UV'
      IF (.NOT.WASOLD) THEN
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *      UTYPE, NLUSER, 'READ', BUFF1, IERR)
         NCFILE = NCFILE + 1
         INFILE = NCFILE
         FVOL(NCFILE) = DISKIN
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         END IF
      SEQOUT = CATBLK(KIIMS)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   CCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1079) IERR
         CALL MSGWRT (8)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, CCNO, DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'UVWIN: ERROR CORRECTING OUTPUT FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Handle coordinates, epochs
      JERR = 0
      GO TO 999
C
 990  IF (JERR.NE.0) THEN
         CALL MSGWRT (8)
      ELSE
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVWIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' USID=',I5)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1070 FORMAT ('UVWIN: ERROR',I3,' UPDATING NEW CATBLK')
 1079 FORMAT ('UVWIN: ERROR',I3,'  COPYING TABLES')
 1200 FORMAT ('There is no OB table to find the orbit. antenna ',
     *   'position')
      END
      SUBROUTINE SENUVW (IRET)
C-----------------------------------------------------------------------
C   SENUVW sends uv data one point at a time to the u, v, w
C   routine and then writes the modified data if requested.
C   Output:
C      IRET   I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'UVFIX.INC'
      CHARACTER PHNAME*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO,
     *   ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, VO, BO,
     *   NUMVIS, XCOUNT, JERR
      REAL      DUM
      INTEGER   IVER, TABBUF(512), NFQID
      INTEGER   FQKOLS(MAXFQC),FQNUMV(MAXFQC),IFQRNO
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA VO, BO /0, 1/
      DATA LUNI, LUNO /56,57/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', FVOL(INFILE), FCNO(INFILE), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Switch to read output file from
C                                       last pass
      INFILE = OUTFIL
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      LRECO = LREC
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       to unravel visibilities we use
C                                       JLOCx parameters from UVPGET.
      IF (JLOCF.EQ.-1) THEN
         NCH = 1
      ELSE
         NCH = CATBLK(KINAX + JLOCF)
         END IF
      IF (JLOCS.EQ.-1) THEN
         NST = 1
      ELSE
         NST = CATBLK(KINAX + JLOCS)
         END IF
      IF (JLOCIF.EQ.-1) THEN
         NIF = 1
      ELSE
         NIF = CATBLK(KINAX + JLOCIF)
         END IF
      REFF    = CATD(KDCRV+JLOCF)
C                                       add by LK 03.21.07
      REFCHN  = CATR(KRCRP+JLOCF)
C                                       now read FQ or CH table
      CALL FNDEXT ('FQ', CATBLK, IVER)
      IF (IVER.GT.0) THEN
         CALL FQINI ('READ', TABBUF, FVOL(INFILE), FCNO(INFILE), IVER,
     *      CATBLK, LUNI, IFQRNO, FQKOLS, FQNUMV, NIF, JERR)
         IF (JERR.NE.0) THEN
            WRITE(MSGTXT,1040) JERR
            CALL MSGWRT(2)
            GO TO 990
            END IF
         NFQID = TABBUF(5)
         CALL TABIO ('CLOS', 0, IFQRNO, TABBUF, TABBUF, JERR)
      ELSE
         NFQID = 1
         END IF
      DO 55 FID = 1, NFQID
         IVER = 1
         CALL CHNDAT ('READ', TABBUF, FVOL(INFILE), FCNO(INFILE), IVER,
     *      CATBLK, LUNI, NIF, FOFF(1,FID), ISBAND(1,FID), FINC(1,FID),
     *      BNDCOD(1,FID), FID, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1050) JERR
            GO TO 990
            END IF
 55      CONTINUE
      IF (NFQID.GT.1) DODOPP = .FALSE.
C                                       Loop
 100  CONTINUE
C                                       visibility addressing
      JNCIF = INCIF / 3
      JNCF  = INCF  / 3
      JNCS  = INCS  / 3
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               IA1 = BUFF1(IPTRI+ILOCB) / 256. + 0.1
               IA2 = BUFF1(IPTRI+ILOCB) - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            FID = 1
            IF (ILOCFQ.GE.0) FID = INT (BUFF1(IPTRI+ILOCFQ))
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            CALL UVWCAL (NUMVIS, BUFF1(IPTRI+ILOCU),
     *         BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *         BUFF1(IPTRI+ILOCT), IA1, IA2, BUFF1(IPTRI+NRPARM),
     *         BUFF1(IPTRI), IRET)
            IF (IRET) 140, 130, 120
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
 130        CONTINUE
               XCOUNT = XCOUNT + 1
C                                       update NX table
               CALL RNXUPD (BUFF1(IPTRI), RNXRET)
               CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
C                                       Skip output
 140        IPTRI = IPTRI + LREC
C                                       Write vis record.
            IF (NIOUT.LT.NIOLIM) GO TO 190
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM,
     *            KBIND, IRET)
               IF (IRET.EQ.0) GO TO 160
                  WRITE (MSGTXT,1140) IRET
                  GO TO 990
 160           IPTRO = KBIND
               NIOUT = 0
 190        CONTINUE
         GO TO 100
C                                       Final call to UVWCAL.
 200  NUMVIS = -1
      CALL UVWCAL (NUMVIS, DUM, DUM, DUM, DUM, IA1, IA2, BUFF1,
     *   BUFF1, IRET)
      IF (IRET.LE.0) GO TO 205
         WRITE (MSGTXT,1120) IRET
         GO TO 990
C                                       Finish write
 205  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1140) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.GT.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      RNXRET = -1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SENUVW: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('SENUVW: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('SENUVW: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SENUVW: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1040 FORMAT ('SENUVW: error ',I3,' Accessing FQ table')
 1050 FORMAT ('SENUVW: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1100 FORMAT ('SENUVW: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SENUVW: UVWCAL ERROR',I3)
 1140 FORMAT ('SENUVW: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE CMPUVW (IRET)
C-----------------------------------------------------------------------
C   CMPUVW sends uv data one point at a time to the u, v, w routine and
C   then writes the modified data if requested.  This routine does the
C   same thing as SENUVW but reads and writes compressed data.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'UVFIX.INC'
      CHARACTER PHNAME*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO,
     *   ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, VO, BO,
     *   NUMVIS, XCOUNT, KLOCWT, KLOCSC, NCORI, JERR, IVER, TABBUF(512),
     *   NFQID, FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFQRNO
      LOGICAL   T, F
      REAL      DUM, TBUFF1(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA VO, BO /0, 1/
      DATA LUNI, LUNO /56,57/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NCORI = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Get input data parms
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), KLOCWT,
     *   JERR)
      IF ((JERR.NE.0) .OR. (KLOCWT.LT.0)) THEN
         IRET = 5
         MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
         GO TO 990
         END IF
      CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), KLOCSC,
     *   JERR)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', FVOL(INFILE), FCNO(INFILE), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Switch to read output file from
C                                       last pass
 10   INFILE = OUTFIL
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LRECO = length of output rec.
 20   LRECO = LREC
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
 40   NUMVIS = 0
      XCOUNT = 0
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       to unravel visibilities we use
C                                       JLOCx parameters from UVPGET.
      IF (JLOCF.EQ.-1) THEN
         NCH = 1
      ELSE
         NCH = CATBLK(KINAX + JLOCF)
         END IF
      IF (JLOCS.EQ.-1) THEN
         NST = 1
      ELSE
         NST = CATBLK(KINAX + JLOCS)
         END IF
      IF (JLOCIF.EQ.-1) THEN
         NIF = 1
      ELSE
         NIF = CATBLK(KINAX + JLOCIF)
         END IF
      REFF    = CATD(KDCRV+JLOCF)
      REFCHN  = CATR(KRCRP+JLOCF)
C                                       now read FQ or CH table
      CALL FNDEXT ('FQ', CATBLK, IVER)
      IF (IVER.GT.0) THEN
         CALL FQINI ('READ',TABBUF, FVOL(INFILE), FCNO(INFILE), IVER,
     *      CATBLK, LUNI, IFQRNO, FQKOLS, FQNUMV, NIF, JERR)
         IF (JERR.NE.0) THEN
            WRITE(MSGTXT,1040) JERR
            CALL MSGWRT(2)
            GO TO 990
            END IF
         NFQID = TABBUF(5)
         CALL TABIO ('CLOS', 0, IFQRNO, TABBUF, TABBUF, JERR)
      ELSE
         NFQID = 1
         END IF
      DO 55 FID = 1, NFQID
         IVER = 1
         CALL CHNDAT ('READ', TABBUF, FVOL(INFILE), FCNO(INFILE), IVER,
     *      CATBLK, LUNI, NIF, FOFF(1,FID), ISBAND(1,FID), FINC(1,FID),
     *      BNDCOD(1,FID), FID, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1050) JERR
            GO TO 990
            END IF
 55      CONTINUE
C                                       Loop
 100  CONTINUE
C                                       visibility addressing
      JNCIF = INCIF
      JNCF  = INCF
      JNCS  = INCS
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
            CALL RCOPY (NRPARM, BUFF1(IPTRI), TBUFF1(1))
            IF (ILOCB.GE.0) THEN
               IA1 = TBUFF1(1+ILOCB) / 256. + 0.1
               IA2 = TBUFF1(1+ILOCB) - IA1*256. + 0.1
            ELSE
               IA1 = TBUFF1(1+ILOCA1) + 0.1
               IA2 = TBUFF1(1+ILOCA2) + 0.1
               END IF
            FID = 1
            IF (ILOCFQ.GE.0) FID = INT (TBUFF1(1+ILOCFQ))
            TBUFF1(IPTRI+ILOCU) = TBUFF1(IPTRI+ILOCU) * UVSCAL
            TBUFF1(IPTRI+ILOCV) = TBUFF1(IPTRI+ILOCV) * UVSCAL
            TBUFF1(IPTRI+ILOCW) = TBUFF1(IPTRI+ILOCW) * UVSCAL
            NUMVIS = NUMVIS + 1
            CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPARM),
     *         BUFF1(IPTRI+KLOCWT), TBUFF1(1+NRPARM))
C                                      Recompute u, v and w
            CALL UVWCAL (NUMVIS, TBUFF1(1+ILOCU), TBUFF1(1+ILOCV),
     *         TBUFF1(1+ILOCW), TBUFF1(1+ILOCT), IA1, IA2,
     *         TBUFF1(1+NRPARM), TBUFF1(1), IRET)
            IF (IRET) 140, 130, 120
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
 130        CONTINUE
               XCOUNT = XCOUNT + 1
               CALL RNXUPD (TBUFF1(1), RNXRET)
               CALL RCOPY (NRPARM, TBUFF1(1), BUFF2(IPTRO))
               CALL ZUVPAK (NCORI, TBUFF1(1+NRPARM),
     *            BUFF2(IPTRO+KLOCWT), BUFF2(IPTRO+NRPARM))
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
C                                       Skip output
 140        IPTRI = IPTRI + LREC
C                                       Write vis record.
            IF (NIOUT.LT.NIOLIM) GO TO 190
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM,
     *            KBIND, IRET)
               IF (IRET.EQ.0) GO TO 160
                  WRITE (MSGTXT,1140) IRET
                  GO TO 990
 160           IPTRO = KBIND
               NIOUT = 0
 190        CONTINUE
         GO TO 100
C                                       Final call to UVWCAL.
 200  NUMVIS = -1
      CALL UVWCAL (NUMVIS, DUM, DUM, DUM, DUM, IA1, IA2, BUFF1,
     *   BUFF1, IRET)
      IF (IRET.LE.0) GO TO 205
         WRITE (MSGTXT,1120) IRET
         GO TO 990
C                                       Finish write
 205  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1140) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, BUFF2, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.GT.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      RNXRET = -1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CMPUVW: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('CMPUVW: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('CMPUVW: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('CMPUVW: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1040 FORMAT ('CMPUVW: error ',I3,' Accessing FQ table')
 1050 FORMAT ('CMPUVW: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1100 FORMAT ('CMPUVW: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('CMPUVW: UVWCAL ERROR',I3)
 1140 FORMAT ('CMPUVW: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE UVWHIS
C-----------------------------------------------------------------------
C   UVWHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      REAL      BEPOCH
      INCLUDE 'UVFIX.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
      DATA LUN1, LUN2 /27,28/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(2), FCNO(1), CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any other history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
         END IF
C                                       Besselian epoch
      IF (BPARM(7).GE.0.0) THEN
         BEPOCH = BPARM(7)
         IF (BEPOCH.LT.1900.0) BEPOCH = 1979.3D0
         WRITE (HILINE,1015) TSKNAM, BEPOCH
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
C                                       close HI
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(1), CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVWHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1015 FORMAT (A6,'BEPOCH =',F8.2,5X,'/ Besselian epoch of obs.')
      END
      SUBROUTINE UVWCAL (NUMVIS, U, V, W, T, IA1, IA2, VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C   UVWCAL on first call sets up to compute u,v,w for a selected array
C   in the data.  All data in this array will have the u, v, w terms
C   recomputed.
C   Inputs:
C      NUMVIS   I       Visibility number, -1 => final call, no data
C                       passed but allows operations to be completed.
C      U        R       U in wavelengths
C      V        R       V in wavelengths
C      W        R       W in wavelengths
C      T        R       Time in days since 0 on the first day for which
C                       there is data.
C      IA1     I        First antenna number
C      IA2     I        Second antenna number
C      RPARM   R(*)     Random parameter array which includes U,V,W etc
C                       but also any other random parameters.
C      VIS     R(3,*)   Visibilities in order real, imaginary, weight
C                       (Jy)
C   Inputs from COMMON
C  APARM(10)  R    Geometry information.
C  BPARM(10)  R    Time information.
C  RA         D    Right ascension (1950) of phase center. (deg)
C  DEC        D    Declination (1950) of phase center. (deg)
C  FREQ       D    Frequency of observation (Hz)
C  NRPARM     I    # random parameters.
C  NCOR       I    # correlators
C  SHIFT(2)   R    Position shift
C  CATBLK(256)I    Catalog header record. See [DOC]HEADER for details
C
C  Output:
C  U          R    U in wavelengths
C  V          R    V in wavelengths
C  W          R    W in wavelengths
C  T          R    Time in same units as input.
C  IA1        I    First antenna number
C  IA2        I    Second antenna number.
C  RPARM      R    Modified random parameter array. NB U,V,W,
C                  time and baseline should not be modified in RPARM
C  VIS        R    Visibilities
C  IRET       I    Return code  -1 => don't write
C                                0 => OK
C                               >0 => error, terminate.
C
C  Output in COMMON
C  NUMHIS     I    # history entries (max. 10)
C  HISCRD(NUMHIS) C History records
C  CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, IRET
      REAL      U, V, W, T, VIS(3,*), RPARM(*), RO, ASEC2M
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  KEYWRD(10)*8, CHSIGN*1, DATE*11
      INTEGER   IROUND, MVIS, LIMIT, LUN, BUFFER(512), IVER, IH(3),
     *   IERR, NANT, I, IBASE, IARR, NUMKEY, LOCS(1), KEYTYP(10),
     *   FOF, IIF, IOF, ICH, IST, IRNO, KOLS(MAXSUC), NUMV(MAXSUC),
     *   NUMIF, VER, NUMREC, LOOP, NUMSOU, ISOU, IFQ, GUSE, FRQSEL
      LOGICAL   DSHFT, TR, FL, CLSORT
      REAL      SEC, DXC, DYC, DZC, XX, TCORR, TTCOR, BASE, POLAR(2)
      DOUBLE PRECISION    JJD, DELDAT, RHOGEO, PHIGEO, RLST, RRA, DDEC,
     *   JD, JD0, DTEMP1, DTEMP2, TU, RATE, GMSTM, C, XLONG, HAM, HR2RD,
     *   TRATE, TC, EPS, DELPSI, DELEPS, SPSI, SEPS, EQEQ, GASTM,
     *   RASTM, RLCOR, UTC, SECS, OBSPOS(3), ZSHIFT, RA0, DEC0
      COMPLEX ZZ, VS
C                                       number of orbiting antennas
C                                       parameters
      INTEGER IORBIT(MAXANT), IORB1, IORB2
      DOUBLE PRECISION  BXM(MAXANT), BYM(MAXANT), BZM(MAXANT), GSTRA,
     *   XG1, YG1, ZG1, XG2, YG2, ZG2, TIME, BASEX, BASEY, BASEZ, VW,
     *   XM1, YM1, ZM1, XM2, YM2, ZM2, TIMEOR, LENGTH, DEANT0, RAANT0,
     *   RAANT, DECANT, XMEQ
      INTEGER IA, ITT(4)
C
      INCLUDE 'UVFIX.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION FC, TRUEF, XRA, XXRA, XDEC, XXDEC, SCALAR,
     *   DDELTA, BEPOCH, R2000, D2000, RORA, DODEC, RO2000, DO2000,
     *   AFREQ, FRQOFF(MAXIF)
      SAVE DXC, DYC, DZC, HR2RD, BXM, BYM, BZM, HAM, TRATE, DSHFT,
     *   MVIS, TTCOR, RLCOR, RRA, DDEC, RASTM, JD, UTC, TCORR, IVER,
     *   SCALAR, NUMSOU, BEPOCH, CLSORT, GUSE, FRQSEL, FRQOFF,
     *   OBSPOS, POLAR, IORBIT
      DATA LUN /29/
      DATA JD0, C /2415020.0D0, 2.9979250D8/
      DATA TR, FL /.TRUE.,.FALSE./
C                                       replaced values
C     DATA VLABX, VLABY, VLABZ /-1601162.D0,-5042003.D0,3554915.D0/
C     DATA VLABX, VLABY, VLABZ /-1601185.365D0, -5041977.547D0,
C    *   3554875.87D0/
      DATA DELDAT /0.05D0/
      DATA KEYWRD /'FREQ    ', 'ARRAYX  ', 'ARRAYY  ', 'ARRAYZ  ',
     *   'GSTIA0  ', 'DEGPDY  ', 'DATUTC  ', 'UT1UTC  ',   2*' '/
      DATA KEYTYP /1, 1, 1, 1, 1, 1, 2, 2, 2*0/
      DATA NUMKEY, LOCS /1, 1/
C-----------------------------------------------------------------------
C                                        Check NUMVIS
      IRET = 8
      IF (NUMVIS.LE.0) GO TO 300
      IF (NUMVIS.EQ.1) THEN
         IVER = IROUND (APARM(2))
         IVER = MAX (1, IVER)
         END IF
C                                       Process vis, check array.
      IRET = 0
      IF (ILOCB.GE.0) THEN
         BASE = RPARM(ILOCB+1)
         IBASE = BASE + 0.1
         IARR = 100.0 * (BASE-IBASE) + 1.5
      ELSE
         IARR = RPARM(1+ILOCSA) + 0.1
         END IF
      IF (IARR.NE.IVER) THEN
         NUMVIS = NUMVIS - 1
         GO TO 999
         END IF
C                                       Source and FQ
      IF (ILOCFQ.GE.0) THEN
         IFQ = RPARM(1+ILOCFQ) + 0.01
      ELSE
         IFQ = 1
         END IF
      IF (ISMULT) THEN
         ISOU = RPARM(1+ILOCSU)
      ELSE
         ISOU = 1
         END IF
C                                       Init files, precession
      IF (NUMVIS.EQ.1) THEN
         IRET = 8
         CLSORT = .FALSE.
         GUSE = XINV + 0.1
         FRQSEL = 1
         CALL DFILL (MAXIF, 0.0D0, FRQOFF)
C                                       Set some constants.
         HR2RD  = DG2RAD * 15.0D0
C                                        Read, update AN table.
         CALL ANTINI ('READ', BUFFER, DISKO, CCNO, IVER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANFQID, ANTNIF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1005)
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                        Close AN file.
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reopen write
         CALL ANTINI ('WRIT', BUFFER, DISKO, CCNO, IVER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANFQID, ANTNIF, IERR)
         IF (IERR.NE.0) GO TO 999

C                                       If antenna table freq is zero
C                                        Use header frequency
         IF (SAFREQ.LE.0.1) SAFREQ = FREQ
C                                       use header for subarray 1
C                                       avoids some AN file errors
         IF (IVER.EQ.1) SAFREQ = FREQ
         IF (UVSCAL.NE.1.0D0) SAFREQ = FREQ
C                                       Get number of antennas
         NANT = BUFFER(5)
C                                        Modify frequency
         IF (BPARM(4).GT.0.0) THEN
            SAFREQ = BPARM(4)
            CATD(KDCRV+JLOCF) = SAFREQ
            FREQ = SAFREQ
            END IF
C                                       Update AN table
         CALL TABKEY ('WRIT', KEYWRD(1), NUMKEY, BUFFER, LOCS(1),
     *      SAFREQ, KEYTYP(1), IERR)
         IF (IERR.NE.0) GO TO 999
C                                        Julian Date
         CALL JULDAY (RDATE, JD)
C                                        UTC as a Modified Julian Date
         UTC = JD - 2400000.5
C                                       Get equation of equinoxes.
         TC = (JD - 2433282.423D0) / 36524.21988D0
         EPS = -46.850D0 * TC - 0.0034D0 * TC * TC +
     *      0.0018D0 * TC * TC * TC
         EPS = (84404.84D0 + EPS) / 3600.0D0 / 180.0D0*3.141592658979D0
         CALL NUT4 (JD, DELPSI, DELEPS, SPSI, SEPS)
         EQEQ = DELPSI * COS (EPS)
C                                       Check if shift.
         DSHFT = (SHIFT(1).NE.0.0) .OR. (SHIFT(2).NE.0.0)
         RA0 = INRA0
         DEC0 = INDE0
         ISGMRT = ANAME.EQ.'GMRT'
         IF ((ARRAYC(1).EQ.0.0D0) .AND. (ARRAYC(2).EQ.0.0D0) .AND.
     *      (ARRAYC(3).EQ.0.0D0)) ARRAYC(3) = 1.0E-20
         IA = APARM(1) + 0.50
         IF (ANAME.EQ.'VLA') THEN
            IF (BPARM(2).EQ.0.0) THEN
               CALL LPSEC (UTC, SECS, DATE)
               BPARM(2) = SECS
               WRITE (MSGTXT,1200) BPARM(2)
               CALL MSGWRT (4)
               WRITE (MSGTXT,1210) DATE
               CALL MSGWRT (4)
               END IF
            END IF
C                                        Other, use input if given.
         IF (IA.GE.3) THEN
            DTEMP1 = APARM(3)
            DTEMP2 = APARM(4)
            IF (ABS(DTEMP1+DTEMP2).GT.0.0D0) ARRAYC(1) = DTEMP1 + DTEMP2
            DTEMP1 = APARM(5)
            DTEMP2 = APARM(6)
            IF (ABS(DTEMP1+DTEMP2).GT.0.0D0) ARRAYC(2) = DTEMP1 + DTEMP2
            DTEMP1 = APARM(7)
            DTEMP2 = APARM(8)
            IF (ABS(DTEMP1+DTEMP2).GT.0.0D0) ARRAYC(3) = DTEMP1 + DTEMP2
            END IF
C                                       Update AN table
         CALL TABKEY ('WRIT', KEYWRD(2), NUMKEY, BUFFER, LOCS(1),
     *      ARRAYC(1), KEYTYP(2), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABKEY ('WRIT', KEYWRD(3), NUMKEY, BUFFER, LOCS(1),
     *      ARRAYC(2), KEYTYP(3), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABKEY ('WRIT', KEYWRD(4), NUMKEY, BUFFER, LOCS(1),
     *      ARRAYC(3), KEYTYP(4), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set position for diurnal
C                                       aberration
         OBSPOS(3) = SQRT (ARRAYC(1)*ARRAYC(1) + ARRAYC(2)*ARRAYC(2) +
     *      ARRAYC(3)*ARRAYC(3))
         RHOGEO = OBSPOS(3) / 6378140.0

         PHIGEO = SQRT (ARRAYC(1)*ARRAYC(1) + ARRAYC(2)*ARRAYC(2))
C                                       Test for Earth-centred array.
         IF ((ARRAYC(3) .LT. 1E-6) .AND. (PHIGEO .LT. 1E-6)) THEN
            PHIGEO = 0.0
         ELSE
            PHIGEO = ATAN2 (ARRAYC(3), PHIGEO)
            END IF
         XLONG = 0.0D0
         IF ((ABS(ARRAYC(1)).GE.1.0).OR.(ABS(ARRAYC(2)).GE.1.0))
     *      XLONG =  ATAN2 (-ARRAYC(2), ARRAYC(1))
C                                       XLONG is west longtitute
         OBSPOS(2) = -XLONG
C                                       OBSPOS(2) is east longtitute
C
         XLONG = (XLONG / DG2RAD ) / 15.0D0
         OBSPOS(1) = PHIGEO
Ctemporally
C            POLRXY(1) = -30.12
C            POLRXY(2) = 15.34
C            POLRXY(1) = POLRXY(1)*30.8
C            POLRXY(2) = POLRXY(2)*30.8
C                                       determine whether POLRXY  in
C                                       asec or in meters; LK DEC 2008
C                                       Ranges of POLRXY in meters and
C                                       in asec are not overlapped
C                                       from 1979 till 2009 at least.
C                                       And MAX(RO)[asec] < 0.59995
C   see    http://vlbi.gsfc.nasa.gov/solve_apriori/usno_finals.erp
         RO = SQRT (POLRXY(1)*POLRXY(1) + POLRXY(2)*POLRXY(2))
         IF (RO .GT. 0.6) THEN
            WRITE (MSGTXT,2100) POLRXY(1), POLRXY(2)
            CALL MSGWRT (5)
C                                       POLARS are in meters.
C                                       So recalculate them to asec.
C                                       Use a mean radius of Earth
            ASEC2M = (6367940.0 * DG2RAD) / 3600.0
            POLRXY(1) = POLRXY(1) / ASEC2M
            POLRXY(2) = POLRXY(2) / ASEC2M
            WRITE (MSGTXT,2110) POLRXY(1), POLRXY(2)
            CALL MSGWRT (5)
            END IF
C                                       check if POLRXY are still not
C                                       in asec
C
         RO = SQRT (POLRXY(1)*POLRXY(1) + POLRXY(2)*POLRXY(2))
         IF (RO .GT. 0.6) THEN
            MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            CALL MSGWRT (5)
            MSGTXT = '!The recalculated POLRXY are out of asec range!'
            CALL MSGWRT (5)
            MSGTXT = '!Therefore POLRXY are not in meter or arcsec  !'
            CALL MSGWRT (5)
            MSGTXT = '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            IRET = 1
            GO TO 990
            END IF
C                                       position of the pole for JPREC
C                                       POLRXY are in arcsec
         POLAR(1) = POLRXY(1)
         POLAR(2) = POLRXY(2)
         RLST = 0.0D0
C                                        Compute earth rotation rate.
C                                        and GMST at UT midnight.
         TU = (JD - JD0) / 36525.0D0
         GMSTM = (8640184.542D0/3600.0D0) * TU
         GMSTM = MOD (GMSTM, 24.0D0)
         GMSTM = (6.0D0 + 38.0D0/60.0D0 + 45.836D0/3600D0) +
     *      GMSTM + (0.0929D0/3600.0D0) * TU * TU
         GASTM = GMSTM + (EQEQ / HR2RD)
         RATE = 1.00273790265D0 + 0.589D-10 * TU
C                                        Update header.
         IF (ABS (GSTIA0).LE.1.0D-25) GSTIA0 = GMSTM * 15.0D0
         IF (ABS (DEGPDY).LE.1.0D-25) DEGPDY = RATE * 360.0D0
C                                       Update AN table
         CALL TABKEY ('WRIT', KEYWRD(5), NUMKEY, BUFFER, LOCS(1),
     *      GSTIA0, KEYTYP(5), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABKEY ('WRIT', KEYWRD(6), NUMKEY, BUFFER, LOCS(1),
     *      DEGPDY, KEYTYP(6), IERR)
         IF (IERR.NE.0) GO TO 999
C                                        Get time offsets.
         IF (ABS (BPARM(1)).GT.1.0E-10) UT1UTC = BPARM(1)
         IF (TIMSYS.EQ.'UTC') DATUTC = 0.0
         IF (ABS (BPARM(2)).GT.1.0E-10) DATUTC = BPARM(2)
         TCORR = BPARM(3)
         TTCOR = (UT1UTC - DATUTC - TCORR) / 86400.0 - (IVER-1)*5.0
C                                       Get RA and Dec offsets from
C                                       uv data reference position.
         DXC = SIN (DG2RAD * (RA-RA0)) * COS (DEC * DG2RAD)
         DYC = COS (DEC0 * DG2RAD) * SIN (DEC * DG2RAD) -
     *      SIN (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *      COS ((RA-RA0) * DG2RAD)
         ZSHIFT = SIN (DEC0 * DG2RAD) * SIN (DEC * DG2RAD) +
     *      COS (DEC0 * DG2RAD) * COS (DEC * DG2RAD) *
     *      COS ((RA-RA0) * DG2RAD)
         DZC = TWOPI * (ZSHIFT - 1.0D0)
         DXC = DXC * TWOPI
         DYC = DYC * TWOPI
C                                       Update AN table, DATUTC,UT1UTC
         CALL TABKEY ('WRIT', KEYWRD(7), NUMKEY, BUFFER, LOCS(1),
     *      DATUTC, KEYTYP(7), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABKEY ('WRIT', KEYWRD(8), NUMKEY, BUFFER, LOCS(1),
     *      UT1UTC, KEYTYP(8), IERR)
         IF (IERR.NE.0) GO TO 999
C                                        Get antenna locations.
         DO 10 IA = 1,NANT
            IANRNO = IA
            CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *         ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *         FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 999
            IORBIT(NOSTA) = 0
            IF (BPARM(6) .EQ. NOSTA) THEN
               MNTSTA = 2
               IORBIT(NOSTA) = 1
            ELSE
C
               BXM(NOSTA) = STAXYZ(1)
C                                       The formulae for U,V,W are
C                                       written in RH. But rotating
C                                       antenna vector by GSTRA I
C                                       carried out change the Y sign.
C                                       So the antennas should be given
C                                       at LH!!!!!!
C                                       AIPS now forces RH.
C                                       So sign of Y is changed
               BYM(NOSTA) = -STAXYZ(2)
               BZM(NOSTA) =  STAXYZ(3)
               END IF
 10         CONTINUE
C                                        Close AN file.
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
C                                        Array center longitude.
C                                        HA at midnight.
         HAM = GASTM - XLONG
         TRATE = RATE * 24.0D0
         EPOCH = INEPO
         RRA = INRA * DG2RAD
         DDEC = INDEC * DG2RAD
         NUMSOU = 1
C                                       Besselian epoch
         BEPOCH = BPARM(7)
         IF (BEPOCH.LT.1900.0) BEPOCH = 1979.3D0
         BPARM(7) = -1.0
C                                       single-source
         IF (.NOT.ISMULT) THEN
C                                       recalculate the source positions
            IF (ABS(EPOCH-2000.0D0).GT.0.0001) THEN
C                                       from B1950 to J2000
               IF (ABS(EPOCH-1950.0D0).LT.0.1D0) THEN
                  EPOCH = 2000.0D0
                  MSGTXT = 'Coordinates precessed from B1950 to J2000'
                  CALL MSGWRT (5)
                  BPARM(7) = BEPOCH
                  CALL EPOCHS (RRA, DDEC, BEPOCH, R2000, D2000)
                  RORA = INORA * DG2RAD
                  DODEC = INODE * DG2RAD
                  IF ((RORA.NE.0.0D0) .OR. (DODEC.NE.0.0D0)) THEN
                     CALL EPOCHS (RORA, DODEC, BEPOCH, RO2000, DO2000)
                  ELSE
                     RO2000 = R2000
                     DO2000 = D2000
                     END IF
C                                       from mean to J2000
C                                       full precession
               ELSE
                  EPOCH = 2000.0D0
                  MSGTXT = 'Apparent coordinates precessed to J2000'
                  CALL MSGWRT (5)
                  JJD = JD + T + TTCOR
                  CALL JPRECL (JJD, EPOCH, DELDAT, -1, TR, TR, OBSPOS,
     *               POLAR, R2000, D2000, RRA, DDEC)
                  RORA = INORA * DG2RAD
                  DODEC = INODE * DG2RAD
                  IF ((RORA.NE.0.0D0) .OR. (DODEC.NE.0.0D0)) THEN
                     CALL JPRECL (JJD, EPOCH, DELDAT, -1, TR, TR,
     *                  OBSPOS, POLAR, RO2000, DO2000, RORA, DODEC)
                  ELSE
                     RO2000 = R2000
                     DO2000 = D2000
                     END IF
                  END IF
               SRA(1) = R2000
               SDEC(1) = D2000
               SFREQO(1) = 0.0D0
C                                       update header
               CATR(KREPO) = EPOCH
               CATD(KDORA) = RO2000 / DG2RAD
               CATD(KDODE) = DO2000 / DG2RAD
               CATD(KDCRV+JLOCR) = R2000 / DG2RAD
               CATD(KDCRV+JLOCD) = D2000 / DG2RAD
C                                       already J2000
            ELSE
               SRA(1) = RRA
               SDEC(1) = DDEC
               END IF
C                                       Multi-source
         ELSE
            CATR(KREPO) =  2000.0
            VER = 1
            CALL SOUINI ('WRIT', BUFFER, DISKO, CCNO, VER, CATBLK, LUN,
     *         NUMIF, VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'OPEN'
               GO TO 990
               END IF
            NUMREC = BUFFER(5)
            DO 50 LOOP = 1,NUMREC
               IRNO = LOOP
               CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, IDSOUR,
     *            SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *            DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *            RESTFQ, PMRA, PMDEC, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'READ'
                  GO TO 990
                  END IF
               IF (IDSOUR.GT.MAXSOU) THEN
                  WRITE (MSGTXT,1011) IDSOUR, MAXSOU
                  CALL MSGWRT (7)
                  GO TO 50
                  END IF
               NUMSOU = MAX (NUMSOU, IDSOUR)
               SFREQO(IDSOUR) = FREQO(1)
C                                       from B1950 to J2000
               IF (ABS(EPOCH-1950.0D0).LT.0.1D0) THEN
                  RRA = RAEPO * DG2RAD
                  DDEC = DECEPO * DG2RAD
                  EPOCH = 2000.0D0
                  BPARM(7) = BEPOCH
                  CALL EPOCHS (RRA, DDEC, BEPOCH, R2000, D2000)
                  RAEPO = R2000 / DG2RAD
                  DECEPO = D2000 / DG2RAD
                  MSGTXT = 'Coordinates precessed from B1950 to J2000'
                  IF (LOOP.EQ.1) CALL MSGWRT (5)
C                                       from mean to J2000
C                                       full precession
               ELSE IF (ABS(EPOCH-2000.0D0).GT.0.1D0) THEN
                  EPOCH = 2000.0D0
                  RRA = RAAPP * DG2RAD
                  DDEC = DECAPP * DG2RAD
                  JJD = JD + T + TTCOR
                  CALL JPRECL (JJD, EPOCH, DELDAT, -1, TR, TR, OBSPOS,
     *               POLAR, R2000, D2000, RRA, DDEC)
                  RAEPO = R2000 / DG2RAD
                  DECEPO = D2000 / DG2RAD
                  MSGTXT = 'Apparent coordinates precessed to J2000'
                  IF (LOOP.EQ.1) CALL MSGWRT (5)
                  END IF
C                                       update SU table
               SRA(IDSOUR) = RAEPO * DG2RAD
               SDEC(IDSOUR) = DECEPO * DG2RAD
               IRNO = LOOP
               CALL TABSOU ('WRIT', BUFFER, IRNO, KOLS, NUMV, IDSOUR,
     *            SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *            DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL,
     *            RESTFQ, PMRA, PMDEC, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET, 'READ'
                  GO TO 990
                  END IF
 50            CONTINUE
            CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'CLOS'
               GO TO 990
               END IF
            END IF
         RASTM = GASTM * HR2RD
         RLCOR = 24.0D0 * HR2RD * RATE
C                                       Determine number of visibilities
         LIMIT = CATBLK(KIDIM)
         MVIS = 1
         DO 60 I = 2,LIMIT
            IF (CATBLK(KINAX+I-1).GT.0) MVIS = MVIS * CATBLK(KINAX+I-1)
 60         CONTINUE
C                                       Zero any Rotation.
         CATR(KRCRT+JLOCD) = 0.0
C                                       other effects (not rotation):
C                                       abberation, bending of light by
C                                       the sun.
C
         JJD = JD + T + TTCOR
         DO 70 I = 1,NUMSOU
            CALL JPRECL (JJD, EPOCH, DELDAT, 1, TR, FL, OBSPOS, POLAR,
     *         SRA(I), SDEC(I), SRAPP(I), SDAPP(I))
 70         CONTINUE
C                                       take a point 10 arcsec north to
C                                       calculate the expansion due to
C                                       differential aberation (Lorentz
C                                       contraction)
         IF (BPARM(8) .EQ. 0) THEN
C                                       yes differential aberration
            XRA = SRA(ISOU)
            DDELTA = (10.0/3600.0) * DG2RAD
            XDEC = SDEC(ISOU) + DDELTA
            CALL JPRECL (JJD, EPOCH, DELDAT, 1, TR, FL, OBSPOS, POLAR,
     *         XRA,XDEC, XXRA, XXDEC)
            DTEMP1 = XXDEC - SDAPP(ISOU)
            DTEMP2 = (XXRA - SRAPP(ISOU)) * COS (SDEC(ISOU))
            DTEMP1 = SQRT (DTEMP1*DTEMP1 + DTEMP2*DTEMP2)
            SCALAR = DTEMP1 / DDELTA
         ELSE
C                                       no differential aberration
            SCALAR = 1.0
            END IF
         END IF
C--------------------------------------- end if vis # 1
C                                       Process vis, check array.
      IRET = 0
      IF (DODOPP) THEN
         CALL FRQUPD (DISKIN, OLDCNO, GUSE, IA1, T, ISOU, IARR,
     *      FRQSEL, CATOLD, CLSORT, FRQOFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1070) IRET
            CALL MSGWRT (8)
            DODOPP = .FALSE.
            CALL DFILL (MAXIF, 0.0D0, FRQOFF)
         ELSE IF (IRET.LT.0) THEN
            IF (NDOPER.LE.3) THEN
               CALL TODHMS (T, ITT)
               WRITE (MSGTXT,1071) IA1, ISOU, ITT
               CALL MSGWRT (7)
               END IF
            NDOPER = NDOPER + 1
            CALL DFILL (MAXIF, 0.0D0, FRQOFF)
            END IF
         END IF
      IRET = 0
C                                       total actual IF 1 freq
      AFREQ = SAFREQ + FOFF(1,IFQ) + SFREQO(ISOU) + FRQOFF(1)
C                                       Shift position if necessary.
      IF (DSHFT) THEN
         DO 250 IIF = 1,NIF
            IOF = (IIF - 1) * JNCIF
            DO 240 ICH = 1,NCH
               FOF = (ICH - 1) * JNCF
               TRUEF  = REFF + (ICH-REFCHN) * FINC(IIF,FID) +
     *            FOFF(IIF,FID) + FRQOFF(IIF)
               FC = TRUEF / AFREQ
               XX = FC * (U*DXC + V*DYC + W*DZC)
               ZZ = CMPLX (COS(XX), -SIN(XX))
C
               DO 230 IST = 1,NST
                  I = 1 + FOF + IOF + (IST - 1) * JNCS
                  VS = CMPLX (VIS(1,I), VIS(2,I)) * ZZ
                  VIS(1,I) = REAL (VS)
                  VIS(2,I) = AIMAG (VS)
 230              CONTINUE
 240           CONTINUE
 250        CONTINUE
         END IF
C                                       Precess position.
      JJD = JD + (T + TTCOR)
      RLST = RASTM + T * RLCOR
C                                       corrected time in days
      TIME = T + TTCOR
C                                       Orbiting antenna position is
C                                       determined at OB table at UTC.
C                                       So UT1UTC is eliminated.
      TIMEOR = T
C                                       Leave if not re-doing u,v,w
      IF (BPARM(10).GT.0.0) GO TO 999
C                                        Compute u, v, w
      GSTRA = (HAM + TRATE * TIME) * 15 * DG2RAD
      IORB1 = IORBIT(IA1)
      IORB2 = IORBIT(IA2)
C                                       The first antenna
C                                       find X,Y,Z of the orbiting
C                                       antenna, using fitting polinoms
C                                       found early
      IF (IORB1.GT.0) THEN
         CALL XYZOR (NINTER, TIMEOR, TBEG, TEND, NFIT, KFIT,
     *      AAX, AAY, AAZ, XM1, YM1, ZM1, IRET)
      ELSE
         XG1 = BXM(IA1)
         YG1 = BYM(IA1)
         ZG1 = BZM(IA1)
         XM1 = XG1*COS(GSTRA) + YG1*SIN(GSTRA)
         YM1 = XG1*SIN(GSTRA) - YG1*COS(GSTRA)
         ZM1 = ZG1
C                                       position of the ground based
C                                       antenna found for the
C                                       observation time sky
C                                       coordinate system
C
         LENGTH = SQRT (XM1*XM1 + YM1*YM1 + ZM1*ZM1)
         RAANT = ATAN2 (YM1, XM1)
         IF (LENGTH.EQ.0.D0) THEN
            DECANT = ASIN (0.0)
         ELSE
            DECANT = ASIN (ZM1 / LENGTH)
            END IF
C                                       Let's recalculate the vector
C                                       Center of Earth - antenna to
C                                       the standard epoch frame.
C                                       Apply only rotation to the
C                                       antenna vector.
C                                       We use the private version of
C                                       JPRECL to provide the rotation
C                                       only.
         CALL JPRECL (JJD, EPOCH, DELDAT, -1, FL, TR, OBSPOS, POLAR,
     *      RAANT0, DEANT0, RAANT, DECANT)
         XMEQ = LENGTH * COS(DEANT0)
         XM1 = XMEQ * COS(RAANT0)
         YM1 = XMEQ * SIN(RAANT0)
         ZM1 = LENGTH * SIN(DEANT0)
         END IF
C                                       The second antenna
      IF (IORB2.GT.0) THEN
         CALL XYZOR (NINTER, TIMEOR, TBEG, TEND, NFIT, KFIT,
     *      AAX, AAY, AAZ, XM2, YM2, ZM2, IRET)
      ELSE
         XG2 = BXM(IA2)
         YG2 = BYM(IA2)
         ZG2 = BZM(IA2)
         XM2 = XG2*COS(GSTRA) + YG2*SIN(GSTRA)
         YM2 = XG2*SIN(GSTRA) - YG2*COS(GSTRA)
         ZM2 = ZG2
C                                       position of the ground based
C                                       antenna found for the
C                                       observation time sky
C                                       coordinate system
C
         LENGTH = SQRT (XM2*XM2 + YM2*YM2 + ZM2*ZM2)
         RAANT = ATAN2 (YM2, XM2)
         IF(LENGTH.EQ.0.D0) THEN
            DECANT = ASIN(0.0)
         ELSE
            DECANT = ASIN (ZM2 / LENGTH)
            ENDIF
C                                       Let's recalculate the vector
C                                       Center of Earth - antenna to the
C                                       standard epoch frame
         CALL JPRECL (JJD, EPOCH, DELDAT, -1, FL, TR, OBSPOS, POLAR,
     *      RAANT0, DEANT0, RAANT, DECANT)
         XMEQ = LENGTH * COS(DEANT0)
         XM2 = XMEQ * COS(RAANT0)
         YM2 = XMEQ * SIN(RAANT0)
         ZM2 = LENGTH * SIN(DEANT0)
         END IF
C                                       calculate baseline
      BASEX = XM1 - XM2
      BASEY = YM1 - YM2
      BASEZ = ZM1 - ZM2
C                                       U, V, W in meters
      VW = BASEX * COS(SRA(ISOU)) + BASEY * SIN(SRA(ISOU))
C                                       The following formulae are valid
C                                       for RH coordinate system: RA
C                                       increase from X to Y U,V,W in
C                                       meters.  Project the baseline on
C                                       the J2000 coordinates
      U = -BASEX * SIN(SRA(ISOU)) + BASEY * COS(SRA(ISOU))
      V = -VW * SIN(SDEC(ISOU)) + BASEZ * COS(SDEC(ISOU))
      W = VW * COS(SDEC(ISOU)) + BASEZ * SIN(SDEC(ISOU))
C                                       U,V,W in wavelengths
C                                       at header frequency not real one
      U = U * FREQ / C
      V = V * FREQ / C
      W = W * FREQ / C
C                                       include differential aberation
C                                       (Lorentz contraction)
      U = U * SCALAR
      V = V * SCALAR
C                                       GMRT has reversed phase
C                                       convention
      IF (ISGMRT) THEN
         U = -U
         V = -V
         W = -W
         END IF
      GO TO 999
C                                        Write history.
 300  IRET = 0
      WRITE (HISCRD(1),2000) IVER
      IF (BPARM(10).GT.0.0) WRITE (HISCRD(1),2008) IVER
      DTEMP1 = SAFREQ * 1.0D-9
      WRITE (HISCRD(2),2001) RDATE, DTEMP1
      WRITE (HISCRD(3),2002) ARRAYC(1)
      WRITE (HISCRD(4),2003) ARRAYC(2)
      WRITE (HISCRD(5),2004) ARRAYC(3)
      CALL COORDD (1, GSTIA0, CHSIGN, IH, SEC)
      WRITE (HISCRD(6),2005) IH(1), IH(2), SEC, DATUTC, UT1UTC
      WRITE (HISCRD(7),2006) TCORR
      WRITE (HISCRD(8),2007) SHIFT
      NUMHIS = 7
      IF (DSHFT) NUMHIS = NUMHIS + 1
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('ERROR: Missing AN table!')
 1010 FORMAT ('UVWCAL: ERROR',I3,1X,A,'ING THE OUTPUT SOURCE TABLE')
 1011 FORMAT ('SOURCE NUMBER',I6,' GREATER THAN ',I5,' IGNORED')
 1070 FORMAT ('UVWCAL: ERROR',I4,' READING FO/CL TABLE FOR DOPPLER')
 1071 FORMAT ('WARNING: NO DOPPLER FOR ANTENNA',I3,' SOURCE',I5,' TIME',
     *   I3,'/',2(I2.2,':'),I2.2)
 1200 FORMAT ('default: IAT-UTC = ',F4.1,' seconds')
 1210 FORMAT ('leap second entries updated until: ',A)
 2000 FORMAT ('Array',I3,' U, V, W computation')
 2001 FORMAT ('Obs date=',A8,' Obs freq=',F11.6,' GHz')
 2002 FORMAT ('Array center BX=',1PD20.12,' meters')
 2003 FORMAT ('Array center BY=',1PD20.12,' meters')
 2004 FORMAT ('Array center BZ=',1PD20.12,' meters')
 2005 FORMAT ('GMST(0)=',2I3,F7.3,' data-UTC=',F8.3,' sec UT1-UTC=',
     *   F8.3,' sec')
 2006 FORMAT ('Clock time corrected by',F10.3,' sec')
 2007 FORMAT ('Tangent point shift =',2F15.5,' arcsec')
 2008 FORMAT ('Array',I3,' U, V, W not recomputed')
 2100 FORMAT ('POLRXY are in meters: ', 2F10.4)
 2110 FORMAT ('So recalculate them to arcsec: ', 2F10.4)
      END
      SUBROUTINE XYZOR (NINTER, TIME, TBEG, TEND, NFIT, KFIT, AAX, AAY,
     *   AAZ, XM, YM, ZM, IRET)
C-----------------------------------------------------------------------
C   Routine to calculate X,Y,Z of the satalite having had the fitting
C   polinoms
C   Input:
C      NINTER     I     Number of times intervals
C      TIME       D     Time in days
C      TBEG(INTER)D     Array of fit intervals beginning
C      TEND(INTER)D     Array of fit intervals end
C      NFIT       I     Muximum of number of coefficients at the
C                       fit polinoms
C      KFIT       I(*)  Array of numbers of coefficients at the
C                           fit polinoms
C                           IFIT - the coefficient number
C                           INDEX = IFIT + NFIT * (INTER - 1)
C      AAX(INDEX) D     Array of coefficients of fit polinom to X
C      AAY(INDEX) D     Array of coefficients of fit polinom to Y
C      AAZ(INDEX) D     Array of coefficients of fit polinom to Z
C      IERR       I     Error; 0 => OK
C   Output:
C      XM         D     X coordinates at the RH equatorial system
C      YM         D     Y coordinates at the RH equatorial system
C      ZM         D     Z coordinates at the RH equatorial system
C                       The coordinates are in meters
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NINTER, NFIT, KFIT(*), IRET, IFIT, INTER, INDEX, LFIT
      DOUBLE PRECISION TIME, TBEG(*), TEND(*), AAX(*), AAY(*),
     *   AAZ(*), XM, YM, ZM, DDT, AX, AY, AZ
      IRET = 0
      DO 50 INTER = 1, NINTER
         LFIT = KFIT(INTER)
         IF (TIME .GE. TBEG(INTER)  .AND.
     *      TIME .LE. TEND(INTER)) THEN
            DDT = TIME - TBEG(INTER)
            INDEX = NFIT * (INTER - 1)
            XM = AAX(1 + INDEX)
            YM = AAY(1 + INDEX)
            ZM = AAZ(1 + INDEX)
            IF (LFIT .GT. 1) THEN
               DO 40 IFIT = 2, LFIT
                  AX = AAX(IFIT + INDEX)
                  AY = AAY(IFIT + INDEX)
                  AZ = AAZ(IFIT + INDEX)
                  XM = XM + AX * (DDT ** (IFIT - 1))
                  YM = YM + AY * (DDT ** (IFIT - 1))
                  ZM = ZM + AZ * (DDT ** (IFIT - 1))
   40             CONTINUE
               END IF
            GO TO 60
            END IF
  50     CONTINUE
      IRET = 1
  60  CONTINUE
C
 999  RETURN
      END
      SUBROUTINE LEASQR (NP, N, SUM, SSQ, R, M, X, VX, SSQRES,
     *   VARRES, VARY, FIT, IERR)
C-----------------------------------------------------------------------
C   This is a double precision version of the least square procedure
C   $APLNOT/LEASQR.FOR.
C-----------------------------------------------------------------------
C     LEASQR does the matrix inversion and other necessary tasks
C     involved in a least squares analysis.
C
C     Given:
C          NP        I     Number of parameters.
C          N         R     The number of observations.
C          SUM       D     Error sum.
C          SSQ       D     Square error sum.
C          R(NP)     D     Results vector.
C
C     Given and returned:
C          M(NP,NP)  D     On input, the upper triangular part contains
C                          the design matrix.  This is not changed.
C                          On output, the lower triangular part contains
C                          the covariance matrix.  Diagonal elements of
C                          the covariance matrix are stored in VX.
C
C     Returned:
C          X(NP)     D     Vector holding the least squares solution.
C          VX(NP)    D     Variance of the best fit parameters.
C          SSQRES    D     Sum of squares of the residuals.
C          VARRES    D     Variance of the residuals.
C          VARY      D     Variance of the error values.
C          FIT       D     Goodness of fit parameter, lies between 0
C                          and 1.
C          IERR      I     Error status, 0 means successful.
C                             1 - nonspecific error return,
C                             2 - insufficient degrees of freedom.
C
C     Called:
C          none
C
C     Algorithm:
C          LU-triangular factorization with scaled partial pivoting.
C          The sub-diagonal triangular matrix contains the scaling
C          factors used at each step in the Gaussian elimination.  Row
C          interchanges are recorded in vectors MXS and SXM.
C             During forward substitution, the pivoting and Gaussian
C          elimination operations performed on matrix M are applied to
C          vector R.  Vector X holds the intermediate result.
C             On backward substitution, successive elements of the
C          solution vector, X, are calculated by substitution of the
C          preceding elements into the equations of the upper triangular
C          factorization of the design matrix.
C
C     Notes:
C       1) Strictly speaking, the design matrix will usually contain
C          rows of zeroes and therefore be singular.  This arises if no
C          observations sensitive to a particular parameter have been
C          done.
C             In practice, any such singularities are ignored and the
C          associated parameters remain undetermined.
C
C       2) The covariance matrix is the inverse of M(i,j) multiplied by
C          the variance of the residuals.  It is obtained by forward and
C          backward substitution on the columns of the unit matrix.
C
C       3) Two statement functions, C, and SC have been employed to
C          partially alleviate the problems posed by passing arrays in
C          FORTRAN.  The design/covariance matrix m(i,j) is copied into
C          the working vector s(i).  This is addressed by using C, and
C          SC in an attempt to make it look like the matrix that it
C          actually represents.
C
C       4) The maximum size problem that LEASQR can handle is set by
C          parameter MX.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Sep/29. Code last modified; 1989/Nov/22.
C-----------------------------------------------------------------------
C     Parameter which determines the maximum size problem.
      INTEGER   MX
      PARAMETER (MX = 50)

      INTEGER   C, I, IERR, ITEMP, J, K, MXS(MX), NF, NP, PIVOT,
     *          SXM(MX)
      REAL N
      DOUBLE PRECISION  COLMAX, DTEMP, FIT, M(NP,NP), R(NP),
     *   RESIDU, RLEN, ROWMAX(MX), S(MX*MX), SC, RTEMP, SSQ,
     *   SSQRES, SUM, VARRES, VARY, VX(NP), W(MX), X(NP)
      INCLUDE 'INCS:DMSG.INC'

C     Statement functions for array manipulation, see note 3 above.
      C(I,J)  = NP*(I-1) + J
      SC(I,J) = S(C(I,J))
C-----------------------------------------------------------------------
C  Initialize.
C     Anticipate and return immediately on error.
      IERR = 1

C     Initialize arrays.
      DO 40 I = 1, NP
C        Vector which records row interchanges.
         MXS(I) = I

C        The solution and variance vectors.
         X(I)  = 0.0
         VX(I) = 0.0

C        Copy the design matrix and zero the covariance matrix.
         DO 10 J = 1, I-1
            M(I,J) = 0.0
            S(C(I,J)) = M(J,I)
 10      CONTINUE
         DO 20 J = I, NP
            S(C(I,J)) = M(I,J)
 20      CONTINUE

C        Find the maximum absolute element in each row.
         ROWMAX(I) = 0.0
         DO 30 J = 1, NP
            ROWMAX(I) = MAX(ROWMAX(I), ABS(SC(I,J)))
 30      CONTINUE
 40   CONTINUE

      VARY   = 0.0
      SSQRES = 0.0
      VARRES = 0.0
      FIT    = 0.0
C     Find the number of degrees of freedom.
      NF = N
      DO 60 I = 1, NP
         IF (ROWMAX(I).NE.0.0) THEN
            NF = NF - 1
         ELSE IF (R(I).NE.0.0) THEN
C           Any row of zeroes must extend to the results vector.
            WRITE (MSGTXT,50) I
 50         FORMAT ('LEASQR: Design matrix inconsistency in row',I4)
            CALL MSGWRT (6)
         END IF
 60   CONTINUE

      IF (NF.LE.1) THEN
         WRITE (MSGTXT,70)
 70      FORMAT ('LEASQR: Insufficient degrees of freedom.')
         CALL MSGWRT (6)
         IERR = 2
         RETURN
      END IF
C  Factorize the matrix.
      DO 120 K = 1, NP
C        Check for a row of zeroes.
         IF (ROWMAX(K).EQ.0.0) GO TO 120

C        A non-zero row maximum implies non-zero diagonal element.
         IF (SC(K,K).EQ.0.0) THEN
            WRITE (MSGTXT,50) MXS(K)
            CALL MSGWRT (6)
            GO TO 120
         END IF
C        Decide whether to pivot.
         COLMAX = ABS(SC(K,K))/ROWMAX(K)
         PIVOT = K
         DO 80 I = K+1, NP
            IF (ROWMAX(I).NE.0.0) THEN
               IF (ABS(SC(I,K))/ROWMAX(I).GT.COLMAX) THEN
                  COLMAX = ABS(SC(I,K))/ROWMAX(I)
                  PIVOT = I
               END IF
            END IF
 80      CONTINUE
         IF (PIVOT.GT.K) THEN
C           We must pivot, interchange the rows of the design matrix.
            DO 90 J = 1, NP
               DTEMP = SC(PIVOT,J)
               S(C(PIVOT,J)) = SC(K,J)
               S(C(K,J)) = DTEMP
 90         CONTINUE
C           Don't forget the vector of row maxima.
            DTEMP = ROWMAX(PIVOT)
            ROWMAX(PIVOT) = ROWMAX(K)
            ROWMAX(K) = DTEMP
C           Record the interchange for later use.
            ITEMP = MXS(PIVOT)
            MXS(PIVOT) = MXS(K)
            MXS(K) = ITEMP
         END IF
C        Gaussian elimination.
         DO 110 I = K+1, NP
C           Nothing to do if SC(i,k) is zero.
            IF (SC(I,K).NE.0.0) THEN
C              Save the scaling factor.
               S(C(I,K)) = SC(I,K)/SC(K,K)

C              Subtract rows.
               DO 100 J = K+1, NP
                  S(C(I,J)) = SC(I,J) - SC(I,K)*SC(K,J)
 100           CONTINUE
            END IF
 110     CONTINUE
 120  CONTINUE
C     MXS(i) records which row of M corresponds to row i of SC.
C     SXM(i) records which row of S corresponds to row i of M.
      DO 130 I = 1, NP
         SXM(MXS(I)) = I
 130  CONTINUE
C  Solve the normal equations.
      DO 150 I = 1, NP
C        Forward substitution.
         W(I) = R(MXS(I))
         DO 140 J = 1, I-1
            W(I) = W(I) - SC(I,J)*W(J)
 140     CONTINUE
 150  CONTINUE
      DO 170 I = NP, 1, -1
C        Backward substitution.
         IF (SC(I,I).NE.0.0) THEN
            DO 160 J = I+1, NP
               W(I) = W(I) - SC(I,J)*W(J)
 160        CONTINUE
            W(I) = W(I)/SC(I,I)
         END IF
         X(I) = W(I)
 170  CONTINUE
C     Check that the solution is acceptable.
      RLEN = 0.0
      RESIDU = 0.0
      DO 200 I = 1, NP
         RTEMP = 0.0
         DO 180 J = 1, I-1
            RTEMP = RTEMP + M(J,I)*X(J)
 180     CONTINUE
         DO 190 J = I, NP
            RTEMP = RTEMP + M(I,J)*X(J)
 190     CONTINUE
         RLEN = RLEN + R(I)**2
         RESIDU = RESIDU + (RTEMP - R(I))**2
 200  CONTINUE
      IF (RESIDU.GT.0.001*RLEN) THEN
         WRITE (MSGTXT,210) RESIDU/RLEN
 210     FORMAT ('LEASQR: The solution is discrepant at',E8.1)
         CALL MSGWRT (6)
         RETURN
      END IF
C  Determine goodness-of-fit estimates, and statistical errors.
      SSQRES = SSQ
      DO 220 I = 1, NP
         SSQRES = SSQRES - X(I)*R(I)
 220  CONTINUE
      IF (SSQRES.LT.0.0) SSQRES = 0.0

      VARRES = SSQRES/NF
      VARY = (SSQ - SUM*SUM/N)/(N - 1.0)
      FIT = 1.0
      IF (VARY.NE.0.0) FIT = 1.0 - SSQRES/(SSQ - SUM*SUM/N)
C     Determine the covariance matrix.
      DO 280 K = 1, NP
C        Forward substitution affects only that part of W() below the
C        first non-zero entry.
         DO 230 I = 1, SXM(K)-1
            W(I) = 0.0
 230     CONTINUE
         W(SXM(K)) = 1.0
         DO 250 I = SXM(K)+1, NP
C           Forward substitution.
            W(I) = 0.0
            DO 240 J = SXM(K), I-1
               W(I) = W(I) - SC(I,J)*W(J)
 240        CONTINUE
 250     CONTINUE

         DO 270 I = NP, K, -1
            IF (SC(I,I).NE.0.0) THEN
C              Backward substitution.
               DO 260 J = I+1, NP
                  W(I) = W(I) - SC(I,J)*W(J)
 260           CONTINUE
               W(I) = W(I)/SC(I,I)
            END IF
            IF (I.NE.K) THEN
C              Off diagonal elements of the covariance matrix.
               M(I,K) = VARRES*W(I)
            ELSE IF (I.EQ.K) THEN
C              Diagonal elements of the covariance matrix.
               VX(K)  = VARRES*W(I)
            END IF
 270     CONTINUE
 280  CONTINUE
C                                       Successful completion.
      IERR = 0
      RETURN
      END
      SUBROUTINE POLIN (ARG, FUNC, NFIT, NEQUAT, FITPAR, VARRES, IRET)
C-----------------------------------------------------------------------
C   This is a double precision version of $APLNOT/POLINO.FOR
C   Routine to fit a polinom to the data by Least Square method
C   Input:
C      ARG     D(*)  Array of data arguments
C      FUNC    D(*)  Array of data function
C      NFIT    I     Number of parameters to fit
C      NEQUAT  I     Total number of points at arrays ARG and FUNC
C   Output:
C      FITPAR  D(*)  Array of found parameters of fitting function
C      VARRES  D     SQRT of residuals
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NFIT, NEQUAT, I, IFIT, KFIT, IKFIT, IRET
      REAL     NOBS
      DOUBLE PRECISION SUM, SSQ, R(20), MATR(400), FITPAR(*), VX(20),
     *   SSQRES, VARRES, VARY, FIT, ARG(*), FUNC(*)
      INCLUDE 'INCS:DDCH.INC'
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 10 KFIT = 1, NFIT
            IKFIT = IFIT + (KFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
      DO 80 I = 1, NEQUAT
         NOBS = NOBS + 1
         SUM = SUM + FUNC(I)
         SSQ = SSQ + FUNC(I)*FUNC(I)
C
         DO 60 IFIT = 1, NFIT
            R(IFIT) = R(IFIT) + FUNC(I) * (ARG(I) ** (IFIT - 1))
C                                       calculate upper/right
C                                       triangle of MATR
            DO 40 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) +
     *            (ARG(I) ** (IFIT - 1)) * (ARG(I) ** (KFIT - 1))
 40            CONTINUE
 60         CONTINUE
 80      CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      VARRES = SQRT(VARRES)
C
 999  RETURN
      END
      SUBROUTINE OBFIT (BUFFER, CNO, CATBLK, LUNI, INVER, IERR)
C-----------------------------------------------------------------------
C   Routine to fit polinoms to the X, Y, Z, VX, VY, VZ of OB table
C   Input:
C      BUFFER     I(512)    I/O buffer and related storage, also
C                           defines file is open.
C      CNO        I         Catalog slot number
C      CATBLK     I(256)    Catalog header block
C      LUNI       I         Logical unit number for table I/O
C      INVER      I         OB table number
C   Output: in COMMON /OBTAB/ (UVFIX.INC)
C                           INTER - interval number
C      TBEG(INTER)D         Array of fit intervals beginning
C      TEND(INTER)D         Array of fit intervals end
C                           NFIT - number of coefficients at the
C                                  fit polinom
C                           IFIT - the coefficient number
C                           INDEX = IFIT + NFIT * (INTER - 1)
C      AAX(INDEX) D         Array of coefficients of fit polinom to X
C      AAY(INDEX) D         Array of coefficients of fit polinom to Y
C      AAZ(INDEX) D         Array of coefficients of fit polinom to Z
C      IERR       I     Error; 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'INCS:POBV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER BUFFER(512), CNO, CATBLK(256), LUNI, INVER, IERR
      INTEGER I,  NUMREC, IOBRNI, IANTOB, ISUBOB, OBKOLS(MAXOBC),
     *   OBNUMV(MAXOBC), IPOINT, KINTER, INDEX, IFIT, LFIT
      INTEGER MAXT
      PARAMETER (MAXT = 100)
      INCLUDE 'UVFIX.INC'
      REAL ANGLOB(3), ECLPOB(4), ORIOB
      DOUBLE PRECISION DTINT, DTIMOB, DXYZOB(3), DVELOB(3), TENDA,
     *   TSTA, DTIME(MAXT), XX(MAXT), YY(MAXT), ZZ(MAXT), AX(MAXFIT),
     *   AY(MAXFIT), AZ(MAXFIT), VARRES, DDT, DDDT, T1
C
C                                       initialize OB table
      CALL OBINI ('READ', BUFFER, DISKIN, CNO, INVER, CATBLK, LUNI,
     *   IOBRNI, OBKOLS, OBNUMV, IERR)
      IF (IERR .NE. 0) THEN
         WRITE (MSGTXT,1200) INVER
         GO TO 990
         END IF
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       IPOINT point's number at the
C                                       interval of fitting
      IPOINT = 0
C                                       KINTER - intervals' number
      KINTER = 1
C                                       DTINT - intervals' time in days
      DTINT = 90.0D0 / 86400.0D0
C                                       NFIT number of terms at the
C                                       fitting polinomial
      NFIT = MAXFIT
C                                       It has been selected time of the
C                                       fitting interval equaled 90 sec
C                                       (10 points with 10sec DT at OB)
C
C                                       Number of terms at the fitting
C                                       polinom is 5
      DO 200  I = 1, NUMREC
C                                       read the input table
         CALL TABOB ('READ', BUFFER, IOBRNI, OBKOLS, OBNUMV,
     *      IANTOB, ISUBOB, DTIMOB, DXYZOB, DVELOB, ANGLOB, ECLPOB,
     *      ORIOB, IERR)
         IF (IPOINT .EQ. 0) TSTA = DTIMOB
C                                       find the time interval at the
C                                       OB table
         IF (I .EQ. 1) T1 = DTIMOB
         IF (I .EQ. 2) DDDT = DTIMOB - T1
         IPOINT = IPOINT + 1
         IF (IPOINT .GT. MAXT) THEN
            IERR = 1
            WRITE (MSGTXT, 2200) IPOINT, MAXT
            GO TO 990
            END IF
         XX(IPOINT) = DXYZOB(1)
         YY(IPOINT) = DXYZOB(2)
         ZZ(IPOINT) = DXYZOB(3)
         TENDA = DTIMOB
         DDT = TENDA - TSTA
         DTIME(IPOINT) = DDT + DDDT
         IF (DDT .GE. (DTINT - (DDDT/2))  .OR.  I .EQ. NUMREC) THEN
            IF (KINTER .EQ. 1) DTIME(1) = DTIME(1) + DDDT
            IF (KINTER .GT. MAXINT) THEN
               IERR =1
               WRITE (MSGTXT,2300) KINTER, MAXINT
               GO TO 990
               END IF
C                                       take TBEG and TEND with
C                                       overcovering
            TBEG(KINTER) = TSTA - DDDT
            TEND(KINTER) = TENDA + DDDT
C                                       fit the polinoms to the data
            IF (IPOINT .LE. NFIT) THEN
               KFIT(KINTER) = IPOINT - 1
               IF (KFIT(KINTER) .LE. 0) KFIT(KINTER) = 1
            ELSE
               KFIT(KINTER) = MAXFIT
               END IF
            LFIT = KFIT(KINTER)
            IF (IPOINT .GT. 1) THEN
               CALL POLIN(DTIME, XX, LFIT, IPOINT, AX, VARRES, IERR)
               CALL POLIN(DTIME, YY, LFIT, IPOINT, AY, VARRES, IERR)
               CALL POLIN(DTIME, ZZ, LFIT, IPOINT, AZ, VARRES, IERR)
            ELSE
               AX(1) = XX(1)
               AY(1) = YY(1)
               AZ(1) = ZZ(1)
               END IF
C                                       store the found polinomials
C                                       coefficients at one array for
C                                       all intervals
            INDEX = NFIT * (KINTER - 1)
            DO 150 IFIT = 1, LFIT
               AAX(IFIT + INDEX) = AX(IFIT)
               AAY(IFIT + INDEX) = AY(IFIT)
               AAZ(IFIT + INDEX) = AZ(IFIT)
  150          CONTINUE
            KINTER = KINTER + 1
            IPOINT = 0
            END IF
  200    CONTINUE
C                                       Close input OB table
      CALL TABIO ('CLOS', 0, IOBRNI, BUFFER, BUFFER, IERR)
      IF (IERR .NE. 0) THEN
         WRITE (MSGTXT,1700) INVER
         GO TO 990
         END IF
      NINTER = KINTER - 1
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('ERROR in initialization of OB table ',I3)
 1700 FORMAT ('ERROR closing OB table', I3)
 2200 FORMAT ('Number of points in the fit interval',I5,
     *   ' > MAXT=',I4)
 2300 FORMAT ('Number of intervals in the fit ',I5,
     *   ' > MAXINT=',I4)
      END
      SUBROUTINE JPRECL (JD, EQUIN, DELDAT, DIR, NROT, ROT, OBSPOS,
     *   POLAR, RAMEAN, DECMEA, RAAPP, DECAPP)
C-----------------------------------------------------------------------
C   Routine to precess positions using the Julian IAU 1984 conventions,
C   (i.e. J2000 positions).  Optional corrections can be made for
C   relativistic bending of the light by the sun, diurnal aberation and
C   polar motion.  Proper motion and parallax are assumed negligible.
C   This is a private version of the AIPS routine. The parameters NROT, ROT
C   switch on/off all other effects (not rotation) and rotation.
C   Several bugs which prevented exact returning back to the mean
C   coordinates (DIR=1 => DIR=-1) are fixed by L. Kogan August 8, 2001
C   Inputs:
C     JD       D  Julian date of observation (e.g. 2446754.123445).
C     EQUIN    D  Epoch of mean equinox (e.g. 2000.0)
C     DELDAT   D  Interpolation interval; compute precession etc.
C                 parameters at this interval and do a linear
C                 interpolation (days).
C     DIR      I  1 => convert from mean to apparent;
C                -1 => convert from apparent to mean.
C     NROT     L  If true correct apparent position for all other
C                 effects (not rotation): abberation, the general
C                 relativistic bending of light by the sun.
C     ROT      L  If true correct for rotation, including precesion and
C                 nutation
C     OBSPOS(3)D  Earth centered location of the observations.
C                 If non zero then diurnal aberation corrections are
C                 made for this location.
C                 1 = Latitude (radians)
C                 2 = East longitude
C                 3 = radius from earth center (meter).
C     POLAR(2) R  X and Y position of the pole (arcsec.)
C                 If non zero then the apparent position is corrected
C                 for the position of the pole.
C                 Note: this correction is not desirable if the antenna
C                 positions are corrected for polar motion.
C    Input/Output:
C     RAMEAN   D  Right ascension at the mean epoch (radians).
C     DECMEA   D  Declination at mean epoch.
C     RAAPP    D  Apparent Right ascension at JD and OBSPOS.
C     DECAPP   D  Apparent declination.
C-----------------------------------------------------------------------
      DOUBLE PRECISION JD, EQUIN, DELDAT, OBSPOS(3), RAMEAN, DECMEA,
     *   RAAPP, DECAPP
      REAL      POLAR(2)
      INTEGER   DIR
      LOGICAL NROT, ROT
      INTEGER   ITEMP, I, J, DIROLD
      LOGICAL   DIURN
      DOUBLE PRECISION JDINT(2), PRNMAT(3,3,2), POSO(3,2), VELO(3,2),
     *   RLST, POS(3), WT1, WT2, TU, GMST, TIME, ROTI(3,3), POSOI(3),
     *   VELOI(3), CONST, V(3), BETA, PDOTV, CONST2, OUT(3), RHOGEO,
     *   GMST0, OBSOLD(3)
C
      DOUBLE PRECISION LENGTH, POSSQR, OUTSQR, VSQR, SD, CD, DRA,
     *   DDEC, AYX, AZX, BYX, BZX
      INCLUDE 'INCS:PSTD.INC'
      SAVE JDINT, PRNMAT, POSO, VELO, OBSOLD, DIROLD
      DATA JDINT, OBSOLD /5*0.0D0/
      DATA DIROLD /100/
C-----------------------------------------------------------------------
C                                       Stmt. fn. to compute GMST at
C                                       IAT=0 in radians.
      GMST0 (TU) = MOD (((((((-6.2D-6 * TU) + 0.093104D0) *
     *   TU) + 8640184.812866D0) * TU + 24110.54841D0) * TWOPI /
     *   86400.0D0), TWOPI)
C-----------------------------------------------------------------------
C                                       Convert to rectangular
C                                       coordinates.
      IF (DIR.GT.0) THEN
         POS(1) = COS (RAMEAN) * COS (DECMEA)
         POS(2) = SIN (RAMEAN) * COS (DECMEA)
         POS(3) = SIN (DECMEA)
      ELSE
         POS(1) = COS (RAAPP) * COS (DECAPP)
         POS(2) = SIN (RAAPP) * COS (DECAPP)
         POS(3) = SIN (DECAPP)
         END IF
C                                       New set of interpolation values?
      IF (((JD.LT.JDINT(1)) .OR. (JD.GT.JDINT(2))) .OR.
     *      (DIR.NE.DIROLD) .OR.
     *      ((ABS (OBSPOS(1)-OBSOLD(1)) + ABS (OBSPOS(2)-OBSOLD(2)) +
     *      ABS (OBSPOS(3)-OBSOLD(3))).GT.1.0D-5)) THEN
         JDINT(1) = JD
         JDINT(2) = JD + DELDAT
         OBSOLD(1) = OBSPOS(1)
         OBSOLD(2) = OBSPOS(2)
         OBSOLD(3) = OBSPOS(3)
         DIROLD = DIR
C                                       Beginning of interpolation
C                                       Precession and nutation
         IF (ROT)
     *      CALL JPRENU (DIR, JDINT(1), EQUIN, .TRUE., PRNMAT(1,1,1))
         IF (NROT) THEN
C                                       Aberation and light bending
            DIURN = (ABS (OBSPOS(3)) .GT. 1.0D-5)
            RHOGEO = 0.0
            RLST = 0.0
C                                       Following for diurnal aberation.
            IF (DIURN) THEN
               RHOGEO = OBSPOS(3) / 6378140.0D0
               ITEMP = JDINT(1)
               TIME = (JDINT(1) - ITEMP - 0.5D0) * TWOPI * 1.002737778D0
               TU = (JDINT(1) - 2451545.0D0) / 36525.0D0
               GMST = GMST0 (TU) + TIME
               RLST = OBSPOS(2) + GMST
               END IF
            CALL JABER (JDINT(1), EQUIN, DIURN, RHOGEO, OBSPOS(1),
     *         RLST, POSO(1,1), VELO(1,1))
            END IF
C                                       End on interpolation,
C                                       don't interpolate less than 1
C                                       sec.
         IF (DELDAT.GT.1.15741D-5) THEN
C                                       Precession and nutation
            IF (ROT)
     *         CALL JPRENU (DIR, JDINT(2), EQUIN, .TRUE., PRNMAT(1,1,2))
            IF (NROT) THEN
C                                       Aberation and light bending
               IF (DIURN) THEN
                  ITEMP = JDINT(2)
                  TIME = (JDINT(2) - ITEMP - 0.5D0) * TWOPI *
     *               1.002737778D0
                  TU = (JDINT(2) - 2451545.0D0) / 36525.0D0
                  GMST = GMST0 (TU) + TIME
                  RLST = OBSPOS(2) + GMST
                  END IF
               CALL JABER (JDINT(2), EQUIN, DIURN, RHOGEO, OBSPOS(1),
     *               RLST,POSO(1,2), VELO(1,2))
               END IF
            END IF
         END IF
C                                       Interpolate vectors and matrix.
      WT1 = 1.0D0
      IF (DELDAT.GT.1.15741D-5)
     *   WT1 = (JDINT(2) - JD) / (JDINT(2) - JDINT(1))
      WT2 = 1.0D0 - WT1
      DO 60 I = 1,3
         IF (NROT) THEN
            POSOI(I) = WT1 * POSO(I,1) + WT2 * POSO(I,2)
            VELOI(I) = WT1 * VELO(I,1) + WT2 * VELO(I,2)
            END IF
         IF (ROT) THEN
            DO 50 J = 1,3
               ROTI(J,I) = WT1 * PRNMAT(J,I,1) + WT2 * PRNMAT(J,I,2)
   50          CONTINUE
            END IF
 60      CONTINUE
C                                       Reduce position.
      IF (DIR.GT.0) THEN
C                                       Mean to apparent.
C                                       Light deflection
C                                       using the routine GRD
         IF (NROT) THEN
            SD = SIN(DECMEA)
            CD = COS(DECMEA)
            CALL GRD (JD, RAMEAN, DECMEA, SD, CD, DRA, DDEC)
            RAAPP = RAMEAN + DRA
            DECAPP= DECMEA + DDEC
            POS(1) = COS (RAAPP) * COS (DECAPP)
            POS(2) = SIN (RAAPP) * COS (DECAPP)
            POS(3) = SIN (DECAPP)
C                                       Aberation
            V(1) = VELOI(1) * 0.0057755D0
            V(2) = VELOI(2) * 0.0057755D0
            V(3) = VELOI(3) * 0.0057755D0
            BETA = SQRT (1.0D0 - (V(1)*V(1) + V(2)*V(2) + V(3)*V(3)))
            PDOTV = POS(1)* V(1) + POS(2)*V(2) + POS(3)*V(3)
C
            CONST = (1.0D0 + (PDOTV / (1.0D0 + BETA))) / (1.0D0 + PDOTV)
            CONST2 = BETA / (1.0D0 + PDOTV)
            POS(1) = POS(1)*CONST2 + CONST*V(1)
            POS(2) = POS(2)*CONST2 + CONST*V(2)
            POS(3) = POS(3)*CONST2 + CONST*V(3)
            OUT(1) = POS(1)
            OUT(2) = POS(2)
            OUT(3) = POS(3)
            END IF
         IF (ROT) THEN
C                                       Precession and nutation
            OUT(1) = POS(1)*ROTI(1,1) + POS(2)*ROTI(2,1) + POS(3)
     *         *ROTI(3,1)
            OUT(2) = POS(1)*ROTI(1,2) + POS(2)*ROTI(2,2) + POS(3)
     *         *ROTI(3,2)
            OUT(3) = POS(1)*ROTI(1,3) + POS(2)*ROTI(2,3) + POS(3)
     *         *ROTI(3,3)
C
            LENGTH = SQRT(OUT(1)*OUT(1) + OUT(2)*OUT(2) + OUT(3)*OUT(3))
            OUT(1) = OUT(1)/LENGTH
            OUT(2) = OUT(2)/LENGTH
            OUT(3) = OUT(3)/LENGTH
            END IF
      ELSE
C                                       Apparent to mean
         OUT(1) = POS(1)
         OUT(2) = POS(2)
         OUT(3) = POS(3)
C
         IF (ROT) THEN
C                                       Precession and nutation
            OUT(1) = POS(1)*ROTI(1,1) + POS(2)*ROTI(2,1) + POS(3)*
     *         ROTI(3,1)
            OUT(2) = POS(1)*ROTI(1,2) + POS(2)*ROTI(2,2) + POS(3)*
     *         ROTI(3,2)
            OUT(3) = POS(1)*ROTI(1,3) + POS(2)*ROTI(2,3) + POS(3)*
     *         ROTI(3,3)
            END IF
         IF (NROT) THEN
C                                       Aberation
            V(1) = VELOI(1) * 0.0057755D0
            V(2) = VELOI(2) * 0.0057755D0
            V(3) = VELOI(3) * 0.0057755D0
C                                       let's calculate LENGTH.
C                                       The vector out*length is equal
C                                       to vector at DIR=1 before
C                                       precession+nutation
            BETA = SQRT (1.0D0 - (V(1)*V(1) + V(2)*V(2) + V(3)*V(3)))
            PDOTV = POS(1)* V(1) + POS(2)*V(2) + POS(3)*V(3)
C
            CONST = (1.0D0 + (PDOTV / (1.0D0 + BETA))) / (1.0D0 + PDOTV)
            CONST2 = BETA / (1.0D0 + PDOTV)
            POSSQR = POS(1)*POS(1) + POS(2)*POS(2) + POS(3)*POS(3)
            OUTSQR = OUT(1)*OUT(1) + OUT(2)*OUT(2) + OUT(3)*OUT(3)
            VSQR = V(1)*V(1) + V(2)*V(2) +V(3)*V(3)
C
            LENGTH = SQRT((POSSQR*CONST2*CONST2 + VSQR*CONST*CONST
     *      + 2*PDOTV*CONST2*CONST) / OUTSQR)
C
            OUT(1) = OUT(1)*LENGTH
            OUT(2) = OUT(2)*LENGTH
            OUT(3) = OUT(3)*LENGTH
C                                       Solve the system of equations
C                                       to find nonaberrated coordinates
C                                       from aberrated ones
            AYX = (OUT(2) - V(2)/(1+BETA)) / (OUT(1) - V(1)/(1+BETA))
            AZX = (OUT(3) - V(3)/(1+BETA)) / (OUT(1) - V(1)/(1+BETA))
            BYX = (AYX*V(1) - V(2)) / (1+BETA)
            BZX = (AZX*V(1) - V(3)) / (1+BETA)
C                                       Solution of the equation
C                                       for OUT(1)
            OUT(1) = ((1 + BYX*V(2) + BZX*V(3))*(OUT(1) - V(1)/(1+BETA))
     *         - (BETA/(1+BETA))*V(1)) / (BETA - (OUT(1) - V(1)/(1+BETA)
     *         )* (V(1) + AYX*V(2) + AZX*V(3)))
C                                       find OUT(2) and OUT(3)
C                                       knowing OUT(1)
            OUT(2) = BYX + AYX*OUT(1)
            OUT(3) = BZX + AZX*OUT(1)
C                                       Light deflection?
C                                       using the routine GRD
            RAAPP = ATAN2 (OUT(2), OUT(1))
            IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + TWOPI
            DECAPP = ASIN (OUT(3))
            SD = SIN(DECAPP)
            CD = COS(DECAPP)
            CALL GRD (JD, RAAPP, DECAPP, SD, CD, DRA, DDEC)
            RAMEAN = RAAPP - DRA
            DECMEA = DECAPP -  DDEC
            OUT(1) = COS (RAMEAN) * COS (DECMEA)
            OUT(2) = SIN (RAMEAN) * COS (DECMEA)
            OUT(3) = SIN (DECMEA)
            END IF
         END IF
C                                       Polar motion
         IF (ROT) THEN
            IF ((ABS (POLAR(1)) + ABS (POLAR(2))).GT.1.0E-20)
     *         CALL JPOLAR (DIR, POLAR(1), POLAR(2), OUT)
            END IF
C                                       Convert to spherical coord.
      IF (DIR.GT.0) THEN
C                                       Mean to apparent
         RAAPP = ATAN2 (OUT(2), OUT(1))
         IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + TWOPI
         DECAPP = ASIN (OUT(3))
      ELSE
C                                       Apparent to mean
         RAMEAN = ATAN2 (OUT(2), OUT(1))
         IF (RAMEAN.LT.0.0D0) RAMEAN = RAMEAN + TWOPI
         DECMEA = ASIN (OUT(3))
         END IF
C
 999  RETURN
      END
      SUBROUTINE EPOCHS (R1950, D1950, BEPOCH, R2000, D2000)
C-----------------------------------------------------------------------
C   Routine to convert B1950.0 coordinates to J2000.0
C   Input:
C      R1950,D1950 D     B1950.0 RA, Dec (rad)
C      BEPOCH      D     Besselian epoch (e.g. 1979.3D0)
C   Output:
C      R2000,D2000 D     J2000.0 RA, Dec (rad)
C-----------------------------------------------------------------------
      DOUBLE PRECISION R1950, D1950, BEPOCH, R2000, D2000
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)
      DOUBLE PRECISION W, EPB2D
      INTEGER I,J
C                                       Position and position+velocity
C                                       vectors
      DOUBLE PRECISION R0(3),A1(3),V1(3),V2(6)
C                                       Radians per year to arcsec per
C                                       century
      DOUBLE PRECISION PMF
      PARAMETER (PMF=100D0*60D0*60D0*360D0/D2PI)
C
      DOUBLE PRECISION A(3), AD(3), EM(6,3), R, COSB
      DATA A, AD/ -1.62557D-6,  -0.31919D-6, -0.13843D-6,
     *           +1.245D-3,    -1.580D-3,   -0.659D-3/
C
      DATA (EM(I,1),I=1,6) / +0.9999256782D0,
     *                       +0.0111820610D0,
     *                       +0.0048579479D0,
     *                       -0.000551D0,
     *                       +0.238514D0,
     *                       -0.435623D0 /
C
      DATA (EM(I,2),I=1,6) / -0.0111820611D0,
     *                       +0.9999374784D0,
     *                       -0.0000271474D0,
     *                       -0.238565D0,
     *                       -0.002667D0,
     *                       +0.012254D0 /
C
      DATA (EM(I,3),I=1,6) / -0.0048579477D0,
     *                       -0.0000271765D0,
     *                       +0.9999881997D0,
     *                       +0.435739D0,
     *                       -0.008541D0,
     *                       +0.002117D0 /
C-----------------------------------------------------------------------
C                                       Spherical to Cartesian
      COSB = COS(D1950)
      R0(1) = COS(R1950) * COSB
      R0(2) = SIN(R1950) * COSB
      R0(3) = SIN(D1950)
C                                       Adjust vector A to give zero
C                                       proper motion in FK5
      W=(BEPOCH - 1950D0) / PMF
      DO 10, I = 1, 3
         A1(I) = A(I) + W * AD(I)
   10    CONTINUE

C                                       Remove e-terms
      W = R0(1) * A1(1) + R0(2) * A1(2) + R0(3) * A1(3)
      DO 20 I = 1, 3
         V1(I) = R0(I) - A1(I) + W * R0(I)
   20    CONTINUE
C                                       Convert position vector to
C                                       Fricke system
      DO 40 I = 1, 6
         W = 0.0D0
         DO 30 J = 1,3
            W = W + EM(I,J) * V1(J)
   30       CONTINUE
         V2(I)=W
   40    CONTINUE
C                                       Allow for fictitious proper
C                                       motion in FK4
      EPB2D = 15019.81352D0 + (BEPOCH - 1900D0) * 365.242198781D0
C
      W = ((EPB2D - 51544.5D0) / 365.25D0) / PMF
      DO 50 I=1,3
         V2(I) =V2(I) + W * V2(I + 3)
   50    CONTINUE
C                                       Revert to spherical coordinates
      R = SQRT(V2(1) * V2(1) + V2(2) * V2(2))
      IF (R .EQ. 0D0) THEN
         W = 0D0
      ELSE
         W = ATAN2(V2(2),V2(1))
         END IF
      IF (V2(3) .EQ. 0D0) THEN
         D2000 = 0D0
      ELSE
         D2000 = ATAN2(V2(3), R)
         END IF
      R2000 = MOD(W, D2PI)
      IF (R2000 .LT. 0D0) R2000 = R2000 + D2PI
C
 999  RETURN
      END
