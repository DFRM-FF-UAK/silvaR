# Data sources and terms of reuse

The example spatial data shipped with **silvaR** come from two public Polish
sources. Both are redistributed here under the terms set out below. Scripts
reproducing each file live in `raw-data/`.

## Canopy height model - `raster/chm.tif`

Derived from airborne laser scanning point clouds published by the Head Office
of Geodesy and Cartography (Glowny Urzad Geodezji i Kartografii, GUGiK) in the
"Dane pomiarowe LIDAR 2023" collection.

| | |
|---|---|
| Tile | M-34-52-B-b-1-1-2 |
| Acquired | 11 May 2023 |
| Published | 26 September 2023 |
| Point density | 25.6 points/m2 as delivered |
| Coordinate system | PL-1992 (EPSG:2180), heights PL-EVRF2007-NH |
| Obtained from | https://opendata.geoportal.gov.pl |

The point cloud was processed into a 1 m canopy height model as documented in
`raw-data/create_chm.R`. GUGiK data are made available as public open data
through the national geoportal; the source, its acquisition date and the
processing applied are recorded above and in that script.

## Forest stand boundaries - `vector/stands.gpkg`

Derived from the stand subarea layer (`G_SUBAREA`) of the Polish Forest Data
Bank (Bank Danych o Lasach, BDL), operated for the State Forests National
Forest Holding by the Bureau for Forest Management and Geodesy (Biuro
Urzadzania Lasu i Geodezji Lesnej, BULiGL).

| | |
|---|---|
| Dataset | BDL_02_15_KONIECPOL_2023 |
| Regional directorate | 02 Katowice |
| Forest district | 15 Koniecpol |
| State of data | 2023, produced 12 June 2023 |
| Obtained from | https://www.bdl.lasy.gov.pl/portal/udostepnianie |

Seven Scots pine subareas of compartments 151 and 152 were selected and the
attribute set reduced, as documented in `raw-data/create_stands.R`.

### Conditions attached by BDL

The Forest Data Bank sets three obligations on anyone reusing its information.
They are reproduced here in substance:

1. State the source of the information, when it was produced and when it was
   obtained. This document does so above.
2. Make the information available onward to other users in the form in which it
   was originally obtained from BDL. The unmodified `G_SUBAREA` layer remains
   available from the BDL portal at the address given above; the copy shipped
   here is a documented subset, not a replacement.
3. Inform the State Forests Directorate General and the BDL operator when the
   reused information has been processed. Processing applied here is the
   subarea selection and attribute reduction described in
   `raw-data/create_stands.R`; notification goes to bdl@bdl.lasy.gov.pl.

Organisational units of the State Forests and the operator of BDL accept no
civil liability for the reuse of information obtained from BDL, to the extent
permitted by law.

## Note on time consistency

Both sources describe the year 2023, and the laser scanning postdates the stand
records by roughly two months. This matters for the `th_calc()` example: where
canopy data and stand records come from different years, recently harvested or
replanted subareas return top heights belonging to the previous stand. Within
the seven pine subareas shipped here the age-to-top-height relationship is
monotonic and correlates at r = 0.93.

## Licence of the package

These conditions concern the data files only. The **silvaR** source code is
distributed under GPL-3, as stated in `DESCRIPTION`.
