-- Unit-of-measure
CREATE VIEW wkt2_uom AS
  SELECT uom_code, unit_of_meas_name, unit_of_meas_type, target_uom_code,
		     factor_b, factor_c
  FROM epsg_unitofmeasure
  WHERE deprecated = 0;

-- Prime meridian
CREATE VIEW wkt2_pm AS
  SELECT prime_meridian_code, prime_meridian_name, greenwich_longitude, uom_code
  FROM epsg_primemeridian
  WHERE deprecated = 0;

-- Ellipsoid
CREATE VIEW wkt2_ell AS
  SELECT ellipsoid_code, ellipsoid_name, semi_major_axis, semi_minor_axis,
         inv_flattening, uom_code
  FROM epsg_ellipsoid
  WHERE deprecated = 0
  ORDER BY ellipsoid_code;

-- Datum
-- If frame_reference_epoch is not NULL then the datum is dynamic
CREATE VIEW wkt2_datum AS
  SELECT datum_code, datum_name, datum_type, ellipsoid_code, prime_meridian_code,
		     frame_reference_epoch
  FROM epsg_datum
  WHERE deprecated = 0 AND datum_type IN ('geodetic', 'dynamic geodetic', 'vertical', 'ensemble')
  ORDER BY datum_code;

CREATE VIEW wkt2_datum_ensemble AS
  SELECT m.datum_ensemble_code, m.datum_sequence, m.datum_code AS member_code,
  			d.datum_name AS member_name, e.ensemble_accuracy, d.datum_type
  FROM epsg_datumensemblemember m
  JOIN epsg_datumensemble e USING (datum_ensemble_code)
  JOIN epsg_datum d USING (datum_code)
  ORDER BY m.datum_ensemble_code, m.datum_sequence;

-- Projected CRS
-- coord_ref_sys_name is unique
CREATE VIEW wkt2_proj_crs AS
	SELECT coord_ref_sys_code, coord_ref_sys_name, coord_sys_code, base_crs_code,
	       projection_conv_code
	FROM epsg_coordinatereferencesystem
	WHERE coord_ref_sys_kind = 'projected' AND deprecated = 0
	ORDER BY coord_ref_sys_code;

-- Geographic and geocentric CRS
-- Combination of coord_ref_sys_name && coord_sys_code is unique. coord_sys_code
-- will determine which CRS kind to use: geographic 2D, geographic 3D, geocentric
CREATE VIEW wkt2_geo_crs AS
	SELECT coord_ref_sys_code, coord_ref_sys_name, coord_ref_sys_kind,
		     coord_sys_code, datum_code, base_crs_code, projection_conv_code
	FROM epsg_coordinatereferencesystem
	WHERE coord_ref_sys_kind LIKE 'geo%' AND deprecated = 0
	ORDER BY coord_ref_sys_code;

-- Vertical CRS
-- coord_ref_sys_name is unique
CREATE VIEW wkt2_vert_crs AS
  SELECT coord_ref_sys_code, coord_ref_sys_name, coord_sys_code, datum_code,
         base_crs_code, projection_conv_code
  FROM epsg_coordinatereferencesystem
  WHERE coord_ref_sys_kind = 'vertical' AND deprecated = 0;

-- Compound CRS
-- coord_ref_sys_name is unique
CREATE VIEW wkt2_cmpd_crs AS
	SELECT coord_ref_sys_code, coord_ref_sys_name, cmpd_horizcrs_code,
	       cmpd_vertcrs_code
	FROM epsg_coordinatereferencesystem
	WHERE coord_ref_sys_kind = 'compound' AND deprecated = 0
	AND cmpd_horizcrs_code IN
	  (SELECT coord_ref_sys_code FROM wkt2_proj_crs
	   UNION -- This removes some spurious compound CRS entries
	   SELECT coord_ref_sys_code FROM wkt2_geo_crs)
	ORDER BY coord_ref_sys_code;

-- Coordinate system
CREATE VIEW wkt2_cs AS
  SELECT coord_sys_code, coord_sys_name, coord_sys_type, dimension
  FROM epsg_coordinatesystem
  WHERE deprecated = 0
  AND coord_sys_code IN (
	  SELECT DISTINCT(coord_sys_code) FROM wkt2_geo_crs
	  UNION
	  SELECT DISTINCT(coord_sys_code) FROM wkt2_proj_crs
	  UNION
	  SELECT DISTINCT(coord_sys_code) FROM wkt2_vert_crs
  );

-- Coordinate system axes
-- note the ordering of coord_sys_code + coord_axis_order
CREATE VIEW wkt2_axis AS
  SELECT ax.coord_axis_code, nm.coord_axis_name, ax.coord_sys_code,
         ax.coord_axis_order, ax.coord_axis_orientation,
         ax.coord_axis_abbreviation, ax.uom_code
  FROM epsg_coordinateaxis ax
  JOIN epsg_coordinateaxisname nm USING (coord_axis_name_code)
  WHERE ax.coord_sys_code IN
	  (SELECT DISTINCT(coord_sys_code) FROM wkt2_cs)
  ORDER BY ax.coord_sys_code, ax.coord_axis_order;

-- Projection methods
CREATE VIEW wkt2_proj_coord_ops AS
  SELECT op.coord_op_code, op.coord_op_name, op.coord_op_method_code,
         meth.coord_op_method_name
  FROM epsg_coordoperation op
  JOIN epsg_coordoperationmethod meth USING (coord_op_method_code)
  WHERE op.deprecated = 0
    AND op.coord_op_type = 'conversion';

-- Projection method parameter values
CREATE VIEW wkt2_proj_coord_ops_values AS
  SELECT v.coord_op_code, v.coord_op_method_code, p.parameter_code,
         p.parameter_name, v.parameter_value, u.sort_order, v.uom_code
  FROM epsg_coordoperationparamvalue v
  JOIN epsg_coordoperationparam p USING (parameter_code)
  JOIN epsg_coordoperationparamusage u USING (coord_op_method_code, parameter_code)
  WHERE v.coord_op_code IN
    (SELECT DISTINCT(coord_op_code) FROM wkt2_proj_coord_ops)
  ORDER BY v.coord_op_code, u.sort_order;

-- UOM aliases
CREATE VIEW wkt2_uom_alias AS
  SELECT object_code, alias
  FROM epsg_alias
  WHERE object_table_name = 'epsg_unitofmeasure'
