package com.cf.parking.api.request;

import lombok.Data;

@Data
public class ParkingFreeReq{

    private String startDate;
    private String endDate;
}
