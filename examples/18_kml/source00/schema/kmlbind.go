// Code generated by xgen. DO NOT EDIT.

package schema

import (
	"encoding/xml"
)

// Anglepos90 ...
type Anglepos90 float64

// Angle90 ...
type Angle90 float64

// Angle180 ...
type Angle180 float64

// Angle360 ...
type Angle360 float64

// AltitudeModeEnum ...
type AltitudeModeEnum string

// Color is aabbggrr
//
//         ffffffff: opaque white
//         ff000000: opaque black
type Color []byte

// Vec2Type ...
type Vec2Type struct {
	XMLName    xml.Name `xml:"vec2Type"`
	XAttr      float64  `xml:"x,attr,omitempty"`
	YAttr      float64  `xml:"y,attr,omitempty"`
	XunitsAttr string   `xml:"xunits,attr,omitempty"`
	YunitsAttr string   `xml:"yunits,attr,omitempty"`
}

// UnitsEnum ...
type UnitsEnum string

// Object ...
type Object *ObjectType

// IdAttributes ...
type IdAttributes struct {
	XMLName      xml.Name `xml:"idAttributes"`
	IdAttr       string   `xml:"id,attr,omitempty"`
	TargetIdAttr string   `xml:"targetId,attr,omitempty"`
}

// ObjectType ...
type ObjectType struct {
	KmlIdAttributes *IdAttributes
}

// Feature ...
type Feature *FeatureType

// FeatureType ...
type FeatureType struct {
	Name             string               `xml:"name"`
	Visibility       bool                 `xml:"visibility"`
	Open             bool                 `xml:"open"`
	Address          string               `xml:"address"`
	PhoneNumber      string               `xml:"phoneNumber"`
	Snippet          *SnippetType         `xml:"Snippet"`
	Description      string               `xml:"description"`
	KmlLookAt        *LookAtType          `xml:"kml:LookAt"`
	KmlTimePrimitive *TimePrimitiveType   `xml:"kml:TimePrimitive"`
	KmlStyleUrl      string               `xml:"kml:styleUrl"`
	KmlStyleSelector []*StyleSelectorType `xml:"kml:StyleSelector"`
	KmlRegion        *RegionType          `xml:"kml:Region"`
	Metadata         *MetadataType        `xml:"Metadata"`
	*ObjectType
}

// SnippetType ...
type SnippetType struct {
	MaxLinesAttr int    `xml:"maxLines,attr,omitempty"`
	Value        string `xml:",chardata"`
}

// LookAt ...
type LookAt *LookAtType

// LookAtType ...
type LookAtType struct {
	Longitude    float64 `xml:"longitude"`
	Latitude     float64 `xml:"latitude"`
	Altitude     float64 `xml:"altitude"`
	Range        float64 `xml:"range"`
	Tilt         float64 `xml:"tilt"`
	Heading      float64 `xml:"heading"`
	AltitudeMode string  `xml:"altitudeMode"`
	*ObjectType
}

// MetadataType ...
type MetadataType struct {
}

// ContainerType ...
type ContainerType struct {
	*FeatureType
}

// Geometry ...
type Geometry *GeometryType

// GeometryType ...
type GeometryType struct {
	*ObjectType
}

// OverlayType ...
type OverlayType struct {
	Color     []byte    `xml:"color"`
	DrawOrder int       `xml:"drawOrder"`
	KmlIcon   *LinkType `xml:"kml:Icon"`
	*FeatureType
}

// StyleSelector ...
type StyleSelector *StyleSelectorType

// StyleSelectorType ...
type StyleSelectorType struct {
	*ObjectType
}

// TimePrimitive ...
type TimePrimitive *TimePrimitiveType

// TimePrimitiveType ...
type TimePrimitiveType struct {
	*ObjectType
}

// ColorStyleType ...
type ColorStyleType struct {
	Color     []byte `xml:"color"`
	ColorMode string `xml:"colorMode"`
	*ObjectType
}

// ColorModeEnum ...
type ColorModeEnum string

// Kml ...
type Kml *KmlType

// KmlType ...
type KmlType struct {
	NetworkLinkControl *NetworkLinkControlType `xml:"NetworkLinkControl"`
	KmlFeature         *FeatureType            `xml:"kml:Feature"`
}

// Document ...
type Document *DocumentType

// DocumentType ...
type DocumentType struct {
	KmlFeature []*FeatureType `xml:"kml:Feature"`
	*ContainerType
}

// Folder ...
type Folder *FolderType

// FolderType ...
type FolderType struct {
	KmlFeature []*FeatureType `xml:"kml:Feature"`
	*ContainerType
}

// Placemark ...
type Placemark *PlacemarkType

// PlacemarkType ...
type PlacemarkType struct {
	KmlGeometry *GeometryType `xml:"kml:Geometry"`
	*FeatureType
}

// NetworkLink ...
type NetworkLink *NetworkLinkType

// NetworkLinkType is Url deprecated in 2.1.
type NetworkLinkType struct {
	RefreshVisibility bool      `xml:"refreshVisibility"`
	FlyToView         bool      `xml:"flyToView"`
	KmlLink           *LinkType `xml:"kml:Link"`
	Url               *LinkType `xml:"Url"`
	*FeatureType
}

// Region ...
type Region *RegionType

// RegionType ...
type RegionType struct {
	KmlLatLonAltBox *LatLonAltBoxType `xml:"kml:LatLonAltBox"`
	KmlLod          *LodType          `xml:"kml:Lod"`
	*ObjectType
}

// LatLonAltBox ...
type LatLonAltBox *LatLonAltBoxType

// LatLonAltBoxType ...
type LatLonAltBoxType struct {
	MinAltitude  float64 `xml:"minAltitude"`
	MaxAltitude  float64 `xml:"maxAltitude"`
	AltitudeMode string  `xml:"altitudeMode"`
	*LatLonBoxType
}

// Lod ...
type Lod *LodType

// LodType ...
type LodType struct {
	MinLodPixels  float32 `xml:"minLodPixels"`
	MaxLodPixels  float32 `xml:"maxLodPixels"`
	MinFadeExtent float32 `xml:"minFadeExtent"`
	MaxFadeExtent float32 `xml:"maxFadeExtent"`
	*ObjectType
}

// NetworkLinkControlType ...
type NetworkLinkControlType struct {
	MinRefreshPeriod float32       `xml:"minRefreshPeriod"`
	Cookie           string        `xml:"cookie"`
	Message          string        `xml:"message"`
	LinkName         string        `xml:"linkName"`
	LinkDescription  string        `xml:"linkDescription"`
	LinkSnippet      *SnippetType  `xml:"linkSnippet"`
	Expires          *DateTimeType `xml:"expires"`
	Update           *UpdateType   `xml:"Update"`
	LookAt           *LookAtType   `xml:"LookAt"`
}

// Icon ...
type Icon *LinkType

// IconType ...
type IconType struct {
	X int `xml:"x"`
	Y int `xml:"y"`
	W int `xml:"w"`
	H int `xml:"h"`
	*LinkType
}

// Link ...
type Link *LinkType

// LinkType is Not final="#all" to show how IconType extended LinkType in KML 2.0.
type LinkType struct {
	Href            string  `xml:"href"`
	RefreshMode     string  `xml:"refreshMode"`
	RefreshInterval float32 `xml:"refreshInterval"`
	ViewRefreshMode string  `xml:"viewRefreshMode"`
	ViewRefreshTime float32 `xml:"viewRefreshTime"`
	ViewBoundScale  float32 `xml:"viewBoundScale"`
	ViewFormat      string  `xml:"viewFormat"`
	HttpQuery       string  `xml:"httpQuery"`
	*ObjectType
}

// RefreshModeEnum ...
type RefreshModeEnum string

// ViewRefreshModeEnum ...
type ViewRefreshModeEnum string

// GeometryElements ...
type GeometryElements struct {
	XMLName      xml.Name `xml:"geometryElements"`
	Extrude      bool
	Tessellate   bool
	AltitudeMode string
}

// MultiGeometry ...
type MultiGeometry *MultiGeometryType

// MultiGeometryType ...
type MultiGeometryType struct {
	KmlGeometry []*GeometryType `xml:"kml:Geometry"`
	*GeometryType
}

// Coordinates ...
type Coordinates *CoordinatesType

// CoordinatesType ...
type CoordinatesType []string

// Point ...
type Point *PointType

// PointType ...
type PointType struct {
	KmlGeometryElements *GeometryElements
	KmlCoordinates      *CoordinatesType `xml:"kml:coordinates"`
	*GeometryType
}

// LineString ...
type LineString *LineStringType

// LineStringType ...
type LineStringType struct {
	KmlGeometryElements *GeometryElements
	KmlCoordinates      *CoordinatesType `xml:"kml:coordinates"`
	*GeometryType
}

// LinearRing ...
type LinearRing *LinearRingType

// LinearRingType ...
type LinearRingType struct {
	KmlGeometryElements *GeometryElements
	KmlCoordinates      *CoordinatesType `xml:"kml:coordinates"`
	*GeometryType
}

// Polygon ...
type Polygon *PolygonType

// PolygonType ...
type PolygonType struct {
	KmlGeometryElements *GeometryElements
	OuterBoundaryIs     *BoundaryType   `xml:"outerBoundaryIs"`
	InnerBoundaryIs     []*BoundaryType `xml:"innerBoundaryIs"`
	*GeometryType
}

// BoundaryType ...
type BoundaryType struct {
	XMLName    xml.Name        `xml:"boundaryType"`
	LinearRing *LinearRingType `xml:"LinearRing"`
}

// Model ...
type Model *ModelType

// ModelType ...
type ModelType struct {
	AltitudeMode   string           `xml:"altitudeMode"`
	KmlLocation    *LocationType    `xml:"kml:Location"`
	KmlOrientation *OrientationType `xml:"kml:Orientation"`
	KmlScale       *ScaleType       `xml:"kml:Scale"`
	KmlLink        *LinkType        `xml:"kml:Link"`
	*GeometryType
}

// Location ...
type Location *LocationType

// LocationType ...
type LocationType struct {
	Longitude float64 `xml:"longitude"`
	Latitude  float64 `xml:"latitude"`
	Altitude  float64 `xml:"altitude"`
	*ObjectType
}

// Orientation ...
type Orientation *OrientationType

// OrientationType ...
type OrientationType struct {
	Heading float64 `xml:"heading"`
	Tilt    float64 `xml:"tilt"`
	Roll    float64 `xml:"roll"`
	*ObjectType
}

// Scale ...
type Scale *ScaleType

// ScaleType ...
type ScaleType struct {
	X float64 `xml:"x"`
	Y float64 `xml:"y"`
	Z float64 `xml:"z"`
	*ObjectType
}

// GroundOverlay ...
type GroundOverlay *GroundOverlayType

// GroundOverlayType ...
type GroundOverlayType struct {
	Altitude     float64        `xml:"altitude"`
	AltitudeMode string         `xml:"altitudeMode"`
	KmlLatLonBox *LatLonBoxType `xml:"kml:LatLonBox"`
	*OverlayType
}

// LatLonBox ...
type LatLonBox *LatLonBoxType

// LatLonBoxType is Yes, north/south range to 180/-180
type LatLonBoxType struct {
	North    float64 `xml:"north"`
	South    float64 `xml:"south"`
	East     float64 `xml:"east"`
	West     float64 `xml:"west"`
	Rotation float64 `xml:"rotation"`
	*ObjectType
}

// ScreenOverlay ...
type ScreenOverlay *ScreenOverlayType

// ScreenOverlayType ...
type ScreenOverlayType struct {
	OverlayXY  *Vec2Type `xml:"overlayXY"`
	ScreenXY   *Vec2Type `xml:"screenXY"`
	RotationXY *Vec2Type `xml:"rotationXY"`
	Size       *Vec2Type `xml:"size"`
	Rotation   float32   `xml:"rotation"`
	*OverlayType
}

// Style ...
type Style *StyleType

// StyleType ...
type StyleType struct {
	KmlIconStyle    *IconStyleType    `xml:"kml:IconStyle"`
	KmlLabelStyle   *LabelStyleType   `xml:"kml:LabelStyle"`
	KmlLineStyle    *LineStyleType    `xml:"kml:LineStyle"`
	KmlPolyStyle    *PolyStyleType    `xml:"kml:PolyStyle"`
	KmlBalloonStyle *BalloonStyleType `xml:"kml:BalloonStyle"`
	KmlListStyle    *ListStyleType    `xml:"kml:ListStyle"`
	*StyleSelectorType
}

// StyleUrl ...
type StyleUrl string

// StyleMap ...
type StyleMap *StyleMapType

// StyleMapType ...
type StyleMapType struct {
	Pair []*StyleMapPairType `xml:"Pair"`
	*StyleSelectorType
}

// StyleMapPairType ...
type StyleMapPairType struct {
	Key         string `xml:"key"`
	KmlStyleUrl string `xml:"kml:styleUrl"`
}

// StyleStateEnum ...
type StyleStateEnum string

// IconStyle ...
type IconStyle *IconStyleType

// IconStyleType ...
type IconStyleType struct {
	Scale   float32            `xml:"scale"`
	Heading float32            `xml:"heading"`
	Icon    *IconStyleIconType `xml:"Icon"`
	HotSpot *Vec2Type          `xml:"hotSpot"`
	*ColorStyleType
}

// IconStyleIconType ...
type IconStyleIconType struct {
	Href string `xml:"href"`
	*ObjectType
}

// LabelStyle ...
type LabelStyle *LabelStyleType

// LabelStyleType ...
type LabelStyleType struct {
	Scale float32 `xml:"scale"`
	*ColorStyleType
}

// LineStyle ...
type LineStyle *LineStyleType

// LineStyleType ...
type LineStyleType struct {
	Width float32 `xml:"width"`
	*ColorStyleType
}

// PolyStyle ...
type PolyStyle *PolyStyleType

// PolyStyleType ...
type PolyStyleType struct {
	Fill    bool `xml:"fill"`
	Outline bool `xml:"outline"`
	*ColorStyleType
}

// BalloonStyle ...
type BalloonStyle *BalloonStyleType

// BalloonStyleType is color deprecated in 2.1
type BalloonStyleType struct {
	Color     []byte `xml:"color"`
	BgColor   []byte `xml:"bgColor"`
	TextColor []byte `xml:"textColor"`
	Text      string `xml:"text"`
	*ObjectType
}

// ListStyle ...
type ListStyle *ListStyleType

// ListStyleType ...
type ListStyleType struct {
	ListItemType string          `xml:"listItemType"`
	BgColor      []byte          `xml:"bgColor"`
	ItemIcon     []*ItemIconType `xml:"ItemIcon"`
	*ObjectType
}

// ListItemTypeEnum ...
type ListItemTypeEnum string

// ItemIconType ...
type ItemIconType struct {
	State []*ItemIconStateType `xml:"state"`
	Href  string               `xml:"href"`
	*ObjectType
}

// ItemIconStateType ...
type ItemIconStateType []string

// ItemIconStateEnum ...
type ItemIconStateEnum string

// TimeStamp ...
type TimeStamp *TimeStampType

// TimeStampType ...
type TimeStampType struct {
	When *DateTimeType `xml:"when"`
	*TimePrimitiveType
}

// TimeSpan ...
type TimeSpan *TimeSpanType

// TimeSpanType ...
type TimeSpanType struct {
	Begin *DateTimeType `xml:"begin"`
	End   *DateTimeType `xml:"end"`
	*TimePrimitiveType
}

// DateTimeType ...
type DateTimeType struct {
	XMLName    xml.Name `xml:"dateTimeType"`
	GYearMonth string
	GYear      string
	DateTime   string
	Date       string
}

// UpdateType ...
type UpdateType struct {
	TargetHref string         `xml:"targetHref"`
	Create     []*CreateType  `xml:"Create"`
	Delete     []*DeleteType  `xml:"Delete"`
	Change     []*ChangeType  `xml:"Change"`
	Replace    []*ReplaceType `xml:"Replace"`
}

// CreateType ...
type CreateType struct {
	KmlFolder *FolderType `xml:"kml:Folder"`
}

// DeleteType ...
type DeleteType struct {
	KmlFeature []*FeatureType `xml:"kml:Feature"`
}

// ChangeType is Any element ultimately derived from Object.
type ChangeType struct {
	KmlObject        []*ObjectType        `xml:"kml:Object"`
	KmlFeature       []*FeatureType       `xml:"kml:Feature"`
	KmlGeometry      []*GeometryType      `xml:"kml:Geometry"`
	KmlStyleSelector []*StyleSelectorType `xml:"kml:StyleSelector"`
	KmlTimePrimitive []*TimePrimitiveType `xml:"kml:TimePrimitive"`
}

// ReplaceType ...
type ReplaceType struct {
	KmlFeature *FeatureType `xml:"kml:Feature"`
}
