package com.cf.parking.api.response;

import lombok.Data;

import java.util.List;

@Data
public class ParkingFreeRsp {
    private String statDate;

    private List<ParkingFreeItemRsp> itemList;
}
