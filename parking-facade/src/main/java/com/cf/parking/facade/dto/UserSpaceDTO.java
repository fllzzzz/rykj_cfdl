package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author lpy
 * @date 2023-03-27 16:56:05
 * @description 用户车位表
 */
@Data
@Accessors(chain = true)
public class UserSpaceDTO implements Serializable {


    /**
     * 工号
     */
    private String personId;

    /**
     * 姓名
     */
    private String personName;

    /**
     * 车牌号
     */
    private String plateNo;

    private List<UserSpaceValidityDTO> validity;
}
