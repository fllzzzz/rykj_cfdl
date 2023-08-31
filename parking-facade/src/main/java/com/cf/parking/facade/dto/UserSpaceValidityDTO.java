package com.cf.parking.facade.dto;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author: lpy
 * @Date: 2023/03/30
 */
@Data
@Accessors(chain = true)
public class UserSpaceValidityDTO {

    private String parkName;

    private String parkSyscode;

    private List<UserSpaceFuncTimeDTO> functionTime;
}
