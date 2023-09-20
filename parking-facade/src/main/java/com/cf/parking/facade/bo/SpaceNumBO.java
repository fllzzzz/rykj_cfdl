package com.cf.parking.facade.bo;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 停车场数量
 * @author
 * @date 2023/9/20
 */
@Data
@Accessors(chain = true)
public class SpaceNumBO {
    /**
     * 停车库名称
     */
    private String name;

    /**
     * 停车库车位剩余数
     */
    private Long value;
}
