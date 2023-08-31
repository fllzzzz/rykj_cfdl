package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;
/**
 * @author lpy
 * @date 2022/10/19
 */

@Data
@Accessors(chain = true)
public class OrderDetailBO {

    private OrderBO order;

    private UserBO profile;

    private List<PassengerPeerBO> list;

    private Integer cancelType;

    private Integer optType;

    private Integer userType;
}
