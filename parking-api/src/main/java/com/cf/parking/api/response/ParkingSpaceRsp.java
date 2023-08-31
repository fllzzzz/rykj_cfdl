package com.cf.parking.api.response;

import lombok.Data;

import java.util.Date;

@Data
public class ParkingSpaceRsp {

    private String spaceNo;

    private String plateNo;

    private Integer parkFlag;

    private String personName;

    private String personId;

    private Integer state;

    private String parkTime;

    private Date inTime;
}