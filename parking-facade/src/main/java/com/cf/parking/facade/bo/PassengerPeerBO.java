package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author lpy
 * @date 2022/10/19
 */
@Data
@Accessors(chain = true)
public class PassengerPeerBO {
    private Long orderPeerId;

    private Long userId;

    private String jobNumber;

    private String name;

    private String avatar;

    private String mobile;

    /**
     * title:乘客、车主
     */
    private String title;
}
